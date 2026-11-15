import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.LockSupport;

/*
ПРАКТИКА: ОТ CAS ДО МЬЮТЕКСА С ОЧЕРЕДЬЮ

План:
1. Написать spinlock и проверить взаимное исключение.
2. Добавить повторный захват тем же потоком (реентерабельность).
3. Исследовать цену активного ожидания на готовом примере.
4. Заменить busywait блокировкой ожидающего потока.
5. Добавить очередь и поддержать произвольное число ожидающих.

Требования: JDK 17+, без Maven и внешних зависимостей.
Из каталога с файлом:
  java MutexWorkshop.java help
  java MutexWorkshop.java demo
  java MutexWorkshop.java spin
  java MutexWorkshop.java reentrant
  java MutexWorkshop.java parked
  java MutexWorkshop.java queue
  java MutexWorkshop.java all
Формат с расширением .java запускает исходный файл без отдельного javac.

Сначала доступны help и demo. Остальные команды вызывают ваши реализации.
TODO выбрасывают UnsupportedOperationException: это заготовки, а не решения.
Заполните методы в указанном порядке. Обвязку тестов менять не требуется.
Проверки имеют таймаут; их потоки daemon, чтобы ошибка реализации не удерживала JVM.
Не используйте synchronized, ReentrantLock, Semaphore, AQS и готовые lock-классы
ВНУТРИ собственных блокировок. AtomicReference, LockSupport и готовая
ConcurrentLinkedQueue разрешены. В обвязке применяется CountDownLatch.

ОБЩИЙ КОНТРАКТ
lock() захватывает блокировку, unlock() освобождает её только владельцем.
Чужой unlock() должен выбрасывать IllegalMonitorStateException и не менять замок.
Каждый успешный lock() вызывающий код обязан сопровождать unlock() в finally.
Для простоты API не содержит timeout/tryLock/Condition и прерываемого захвата.
Ожидание не прерывается по interrupt: его статус нужно восстановить перед возвратом.
Блокировка одноразовостью не ограничена: экземпляр используется многократно.
AtomicReference<Thread> owner: null — свободно, иначе текущий владелец.
Сравнение потоков — по идентичности объектов (==), не по имени.
CAS(expected, update) атомарно проверяет значение и меняет его только при совпадении.
Один CAS-захват null -> current может выиграть только один поток.

ШАГ 1. SPINLOCK — класс SpinMutex
Сделайте lock() циклом CAS над owner. Пока захват не удался, вызывайте onSpinWait().
onSpinWait() — подсказка процессору, она не усыпляет поток и не освобождает замок.
unlock() должен атомарно заменить current на null; неудача означает чужой unlock.
Повторный lock() владельцем для этого шага определите как IllegalStateException,
чтобы случайный повторный вход не превращался в бесконечное вращение.
Запустите spin: проверяются обычный счётчик, взаимное исключение и чужой unlock.


ШАГ 2. РЕЕНТЕРАБЕЛЬНОСТЬ — класс ReentrantSpinMutex
Скопируйте свой подход шага 1. Если current уже владелец, увеличьте счётчик на 1
захвата без попытки CAS null -> current. Только первый захват конкурирует с другими.
Каждый unlock() уменьшает счётчик; owner становится null только при счётчике 0.
Обычный int depth изменяет только владелец. Нельзя обнулить owner до завершения
работы с depth: другой поток мог бы уже получить замок и начать менять depth.
Проверьте переполнение глубины: отрицательная/нулевая глубина не должна возникнуть
после очередного успешного lock(). Запустите reentrant.


ШАГ 3. ЦЕНА ОЖИДАНИЯ — готовая команда demo
Запустите demo (при желании повторите с аргументом длительности в миллисекундах:
java MutexWorkshop.java demo 1000). Код модели ожидания уже написан ниже.
Поток-"владелец" ещё не разрешил продолжение, ожидающий либо вращается на флаге,
либо park-уется. Через заданное время координатор выставляет флаг и будит ожидающего.
Сравниваем wall time и CPU time самого ожидающего потока. CPU time — сколько
процессорного времени получил поток, а не сколько прошло на часах.
При spinning ожидаем заметный расход CPU; при parking — значительно меньший.
Если JVM не поддерживает CPU time, программа сообщает об этом явно.
Это иллюстрация механики ожидания.
Sleep здесь моделирует задержку разрешения, например ожидание I/O владельцем.
Он намеренно используется координатором, а не как реализация lock().


ШАГ 4. ОДИН ОЖИДАЮЩИЙ — класс OneWaiterMutex
Это намеренно ограниченный промежуточный протокол: не более двух потоков-клиентов
одновременно, значит владелец и максимум один ожидающий. Реентерабельности нет.
Используйте owner и один атомарный слот waiter. Повторный lock владельца — ошибка.

LockSupport.park(blocker) может приостановить текущий поток; blocker — объект для
диагностики, передавайте this. unpark(thread) выдаёт указанному потоку разрешение.
Если unpark случился раньше park, следующий park использует разрешение без сна.
Разрешение максимум одно: несколько unpark не накапливаются как у семафора.
park может вернуться без освобождения замка, в том числе после interrupt.
Поэтому после возврата СНОВА проверяйте возможность CAS-захвата в цикле.
Само пробуждение НЕ передаёт владение! Оно лишь позволяет снова попытаться.

Порядок действий: попытка захвата; регистрация ожидающего; повторная проверка
захвата после регистрации; затем park в цикле. После захвата удалите свою регистрацию.
Освобождающий сначала освобождает owner и затем будит зарегистрированного ожидающего.
Подумайте над двумя случаями: освобождение до регистрации и после неё.
Почему нельзя просто сделать неудачный CAS -> park без регистрации и перепроверки?
Слот очищайте условно (CAS с собой), чтобы не удалить чужую регистрацию.
При interrupt запомните и очистите статус через Thread.interrupted(), продолжайте
ожидать, а после успешного захвата восстановите Thread.currentThread().interrupt().
Иначе park может немедленно возвращаться снова и снова, расходуя CPU.
Запустите parked. Кроме двухпоточного счётчика есть проверка прерывания ожидающего.

ШАГ 5. ОЧЕРЕДЬ — класс QueuedMutex
Снимите ограничение одного ожидающего. Для хранения потоков используйте выданную
ConcurrentLinkedQueue<Thread>; её реализацию писать не нужно.
На этом шаге тоже делаем НЕРЕЕНТЕРАБЕЛЬНЫЙ замок, чтобы сосредоточиться на ожидании.
Каждый претендент, включая первый, регистрируется в очереди ровно один раз.
Пытаться CAS-захватить свободный замок разрешено только голове очереди.
При успехе поток удаляет свою запись из головы, после чего становится владельцем.
Пока захват невозможен — park и повторная проверка. Interrupt обрабатывайте как в шаге 4.
unlock освобождает owner, затем unpark-ует текущую голову очереди, если она есть.
Условие головы проверяйте перед CAS; никто не должен обходить очередь быстрым путём.
Порядок FIFO относится к регистрации в очереди, а не к моменту вызова lock().
ОС всё ещё не обязана немедленно дать процессор разбуженному потоку.
Запустите queue: проверки используют несколько потоков и повторные захваты.

Документация примитивов:
https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/atomic/AtomicReference.html
https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/locks/LockSupport.html
*/
public class MutexWorkshop {
    interface Mutex {
        void lock();
        void unlock();
    }

    static final class SpinMutex implements Mutex {
        private final AtomicReference<Thread> owner = new AtomicReference<>();
        public void lock() {
            Thread current = Thread.currentThread();
            if (owner.get() == current) throw new IllegalStateException("Повторный вход");
            while (!owner.compareAndSet(null, current)) Thread.onSpinWait();
        }
        public void unlock() {
            if (!owner.compareAndSet(Thread.currentThread(), null))
                throw new IllegalMonitorStateException("Не владелец замка");
        }
    }

    static final class ReentrantSpinMutex implements Mutex {
        private final AtomicReference<Thread> owner = new AtomicReference<>();
        private int depth;
        public void lock() {
            Thread current = Thread.currentThread();
            if (owner.get() == current) {
                if (depth == Integer.MAX_VALUE) throw new IllegalStateException("Переполнение глубины");
                depth++;
                return;
            }
            while (!owner.compareAndSet(null, current)) Thread.onSpinWait();
            depth = 1;
        }
        public void unlock() {
            if (owner.get() != Thread.currentThread())
                throw new IllegalMonitorStateException("Не владелец замка");
            if (--depth == 0) owner.set(null); // depth уже обновлён до освобождения
        }
    }

    static final class OneWaiterMutex implements Mutex {
        private final AtomicReference<Thread> owner = new AtomicReference<>();
        private final AtomicReference<Thread> waiter = new AtomicReference<>();
        public void lock() {
            Thread current = Thread.currentThread();
            if (owner.get() == current) throw new IllegalStateException("Повторный вход");
            if (owner.compareAndSet(null, current)) return;
            waiter.set(current);                       // регистрация
            boolean interrupted = false;
            while (!owner.compareAndSet(null, current)) { // перепроверка после регистрации
                LockSupport.park(this);
                if (Thread.interrupted()) interrupted = true;
            }
            waiter.compareAndSet(current, null);
            if (interrupted) current.interrupt();
        }
        public void unlock() {
            if (!owner.compareAndSet(Thread.currentThread(), null))
                throw new IllegalMonitorStateException("Не владелец замка");
            Thread w = waiter.get();
            if (w != null) LockSupport.unpark(w);
        }
    }

    static final class QueuedMutex implements Mutex {
        private final AtomicReference<Thread> owner = new AtomicReference<>();
        private final ConcurrentLinkedQueue<Thread> waiters = new ConcurrentLinkedQueue<>();
        public void lock() {
            Thread current = Thread.currentThread();
            if (owner.get() == current) throw new IllegalStateException("Повторный вход");
            waiters.add(current);
            boolean interrupted = false;
            while (true) {
                if (waiters.peek() == current && owner.compareAndSet(null, current)) {
                    waiters.poll(); // мы голова очереди
                    break;
                }
                LockSupport.park(this);
                if (Thread.interrupted()) interrupted = true;
            }
            if (interrupted) current.interrupt();
        }
        public void unlock() {
            if (!owner.compareAndSet(Thread.currentThread(), null))
                throw new IllegalMonitorStateException("Не владелец замка");
            Thread head = waiters.peek();
            if (head != null) LockSupport.unpark(head);
        }
    }

    private static UnsupportedOperationException todo(int step) {
        return new UnsupportedOperationException("Сначала выполните шаг " + step);
    }

    // ===== Готовая обвязка. Ниже менять код для выполнения заданий не нужно. =====
    private static final long TIMEOUT_MS = 10_000;
    private static final class Counter { long value; }

    private static void require(boolean condition, String message) {
        if (!condition) throw new AssertionError(message);
    }

    private static Thread launch(String name, AtomicReference<Throwable> failure,
                                 Runnable action) {
        Thread t = new Thread(() -> {
            try { action.run(); }
            catch (Throwable e) { failure.compareAndSet(null, e); }
        }, name);
        t.setDaemon(true);
        t.start();
        return t;
    }

    private static void joinAll(List<Thread> threads, AtomicReference<Throwable> failure)
            throws InterruptedException {
        long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(TIMEOUT_MS);
        for (Thread t : threads) {
            while (t.isAlive() && failure.get() == null) {
                long left = deadline - System.nanoTime();
                if (left <= 0) throw new AssertionError("Таймаут: проверьте протокол ожидания");
                t.join(Math.min(100, Math.max(1, TimeUnit.NANOSECONDS.toMillis(left))));
            }
            if (failure.get() != null) throw new AssertionError("Ошибка в рабочем потоке", failure.get());
        }
    }

    private static void await(CountDownLatch latch) {
        try {
            if (!latch.await(TIMEOUT_MS, TimeUnit.MILLISECONDS))
                throw new AssertionError("Таймаут сигнала");
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new AssertionError("Обвязка прервана", e);
        }
    }

    private static void counterTest(Mutex mutex, int threadCount) throws Exception {
        Counter counter = new Counter();
        AtomicInteger inside = new AtomicInteger();
        AtomicReference<Throwable> failure = new AtomicReference<>();
        CountDownLatch start = new CountDownLatch(1);
        List<Thread> threads = new ArrayList<>();
        int iterations = 10_000;
        for (int i = 0; i < threadCount; i++) {
            threads.add(launch("counter-" + i, failure, () -> {
                await(start);
                for (int j = 0; j < iterations; j++) {
                    mutex.lock();
                    try {
                        int count = inside.incrementAndGet();
                        try {
                            require(count == 1, "Одновременно вошли несколько потоков");
                            long old = counter.value;
                            if ((j & 255) == 0) Thread.yield();
                            counter.value = old + 1;
                        } finally { inside.decrementAndGet(); }
                    } finally { mutex.unlock(); }
                }
            }));
        }
        start.countDown();
        joinAll(threads, failure);
        require(counter.value == (long) threadCount * iterations, "Потеря обновлений");
        System.out.println("OK: счётчик=" + counter.value + ", потоков=" + threadCount);
    }

    private static void ownerTest(Mutex mutex) throws Exception {
        AtomicReference<Throwable> failure = new AtomicReference<>();
        mutex.lock();
        try {
            Thread stranger = launch("stranger", failure, () -> {
                boolean rejected = false;
                try { mutex.unlock(); }
                catch (IllegalMonitorStateException expected) { rejected = true; }
                require(rejected, "Чужой unlock должен быть отклонён");
            });
            joinAll(List.of(stranger), failure);
        } finally { mutex.unlock(); }
        boolean rejected = false;
        try { mutex.unlock(); }
        catch (IllegalMonitorStateException expected) { rejected = true; }
        require(rejected, "Повторный unlock свободного замка должен быть отклонён");
        System.out.println("OK: проверка владельца");
    }

    private static void nestedTest(Mutex mutex) throws Exception {
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread t = launch("nested", failure, () -> {
            mutex.lock();
            try {
                mutex.lock();
                try { /* Вложенная критическая секция. */ }
                finally { mutex.unlock(); }
            } finally { mutex.unlock(); }
        });
        joinAll(List.of(t), failure);
        System.out.println("OK: вложенный захват и полное освобождение");
        counterTest(mutex, 2);
    }

    private static void partialReleaseTest(Mutex mutex) throws Exception {
        AtomicReference<Throwable> failure = new AtomicReference<>();
        CountDownLatch attempting = new CountDownLatch(1);
        CountDownLatch entered = new CountDownLatch(1);
        mutex.lock();
        Thread competitor;
        try {
            mutex.lock();
            try {
                competitor = launch("partial-release", failure, () -> {
                    attempting.countDown();
                    mutex.lock();
                    try { entered.countDown(); }
                    finally { mutex.unlock(); }
                });
                await(attempting);
            } finally { mutex.unlock(); }
            require(!entered.await(50, TimeUnit.MILLISECONDS),
                    "Промежуточный unlock освободил реентерабельный замок");
        } finally { mutex.unlock(); }
        joinAll(List.of(competitor), failure);
        require(entered.getCount() == 0, "Последний unlock не освободил замок");
        System.out.println("OK: промежуточный unlock сохраняет владение");
    }

    private static void nonReentrantTest(Mutex mutex) {
        mutex.lock();
        try {
            boolean rejected = false;
            try { mutex.lock(); }
            catch (IllegalStateException expected) { rejected = true; }
            require(rejected, "Повторный вход должен быть явно отклонён");
        } finally { mutex.unlock(); }
        System.out.println("OK: повторный вход отклонён");
    }

    private static void interruptTest(Mutex mutex) throws Exception {
        AtomicReference<Throwable> failure = new AtomicReference<>();
        CountDownLatch attempting = new CountDownLatch(1);
        CountDownLatch acquired = new CountDownLatch(1);
        mutex.lock();
        Thread waiter;
        try {
            waiter = launch("interrupted-waiter", failure, () -> {
                attempting.countDown();
                mutex.lock();
                try {
                    require(Thread.currentThread().isInterrupted(), "Статус interrupt потерян");
                    acquired.countDown();
                } finally { mutex.unlock(); }
            });
            await(attempting);
            waiter.interrupt();
            require(!acquired.await(50, TimeUnit.MILLISECONDS), "Вход при занятом замке");
        } finally { mutex.unlock(); }
        joinAll(List.of(waiter), failure);
        require(acquired.getCount() == 0, "Ожидающий не получил замок");
        System.out.println("OK: interrupt не отменил ожидание, статус сохранён");
    }

    private record WaitingResult(String mode, long wallNanos, long cpuNanos) {}

    private static WaitingResult waitingSample(boolean spin, long millis) throws Exception {
        ThreadMXBean bean = ManagementFactory.getThreadMXBean();
        boolean cpuAvailable = bean.isCurrentThreadCpuTimeSupported();
        if (cpuAvailable && !bean.isThreadCpuTimeEnabled()) {
            try { bean.setThreadCpuTimeEnabled(true); }
            catch (UnsupportedOperationException | SecurityException e) { cpuAvailable = false; }
        }
        final boolean measureCpu = cpuAvailable && bean.isThreadCpuTimeEnabled();
        AtomicBoolean released = new AtomicBoolean();
        AtomicReference<WaitingResult> result = new AtomicReference<>();
        AtomicReference<Throwable> failure = new AtomicReference<>();
        CountDownLatch ready = new CountDownLatch(1);
        Thread waiter = launch("demo-" + (spin ? "spin" : "park"), failure, () -> {
            long cpuStart = measureCpu ? bean.getCurrentThreadCpuTime() : -1;
            long start = System.nanoTime();
            ready.countDown();
            while (!released.get()) {
                if (spin) Thread.onSpinWait();
                else LockSupport.park(released);
            }
            long wall = System.nanoTime() - start;
            long cpuEnd = measureCpu ? bean.getCurrentThreadCpuTime() : -1;
            result.set(new WaitingResult(spin ? "spin" : "park", wall,
                    cpuStart >= 0 && cpuEnd >= 0 ? cpuEnd - cpuStart : -1));
        });
        await(ready);
        try { Thread.sleep(millis); }
        finally {
            released.set(true);
            LockSupport.unpark(waiter);
        }
        joinAll(List.of(waiter), failure);
        return result.get();
    }

    private static void waitingDemo(long millis) throws Exception {
        require(millis >= 50 && millis <= 3000, "Длительность: от 50 до 3000 ms");
        System.out.println("Один ожидающий поток; разрешение продолжить через " + millis + " ms.");
        for (int round = 0; round < 2; round++) {
            for (boolean spin : new boolean[] {true, false}) {
                WaitingResult r = waitingSample(spin, millis);
                String cpu = r.cpuNanos < 0 ? "недоступно" : String.format("%.2f ms", r.cpuNanos / 1e6);
                System.out.printf("%s: wall=%.2f ms, CPU=%s%n", r.mode, r.wallNanos / 1e6, cpu);
            }
        }
        System.out.println("Сравните расход CPU, не небольшие различия wall time.");
    }

    private static void runStep(String step) throws Exception {
        switch (step) {
            case "spin" -> {
                Mutex m = new SpinMutex();
                ownerTest(m); nonReentrantTest(m); counterTest(m, 4);
            }
            case "reentrant" -> {
                Mutex m = new ReentrantSpinMutex();
                ownerTest(m); nestedTest(m); partialReleaseTest(m); counterTest(m, 4);
            }
            case "parked" -> {
                Mutex m = new OneWaiterMutex();
                ownerTest(m); nonReentrantTest(m); counterTest(m, 2); interruptTest(m);
            }
            case "queue" -> {
                Mutex m = new QueuedMutex();
                ownerTest(m); nonReentrantTest(m); counterTest(m, 6); interruptTest(m);
            }
            default -> throw new IllegalArgumentException("Неизвестный шаг: " + step);
        }
    }

    public static void main(String[] args) throws Exception {
        String command = args.length == 0 ? "help" : args[0];
        if (command.equals("help")) {
            System.out.println("Задания и инструкции находятся в комментарии в начале файла.");
            System.out.println("java MutexWorkshop.java demo [50..3000 ms]");
            System.out.println("java MutexWorkshop.java spin|reentrant|parked|queue|all");
            return;
        }
        if (command.equals("demo")) {
            waitingDemo(args.length > 1 ? Long.parseLong(args[1]) : 500);
            return;
        }
        // Инициализируем используемый для парковки класс до начала упражнений.
        LockSupport.unpark(null);
        if (command.equals("all")) {
            for (String step : new String[] {"spin", "reentrant", "parked", "queue"}) {
                System.out.println("=== " + step + " ===");
                runStep(step);
            }
        } else runStep(command);
    }
}
