import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicIntegerArray;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.ReentrantLock;

/*
ПРАКТИКА 1: БЛОКИРУЮЩАЯ ОЧЕРЕДЬ — ЧАСТИ 1–3
1. Потокобезопасная очередь с offer/poll.
2. Блокирующие put/take.
3. Производители, потребители и завершение.

JDK 17+, без Maven. Запуск: java BoundedQueueWorkshop.java help
Команды: deque, try, blocking, demo, transfer, all.
Команда deque работает сразу; остальные — после заполнения TODO.
Обвязка уже готова. Вопросы, инварианты и эксперименты: BoundedQueueDiscussion.md.
Продолжение (части 4–5): отдельный BoundedQueueAdvanced.java.

ГОТОВЫЕ СТРУКТУРЫ И ПРИМИТИВЫ
ArrayDeque<E> — готовое хранилище: addLast добавляет в конец, removeFirst извлекает
первый элемент, size/isEmpty сообщают размер/пустоту. Дека НЕ ПОТОКОБЕЗОПАСНА:
все обращения к общему экземпляру защищайте одним мьютексом.
ArrayDeque сама увеличивает вместимость: наше ограничение capacity нужно соблюдать
отдельно. Начальный размер ArrayDeque не задаёт верхний предел количества элементов.
null запрещён, поэтому poll может вернуть null как признак пустой очереди.
Job(id) — сообщение примера; id >= 0 — работа, id == -1 — маркер завершения.
TryQueue<E> — заготовка шага 1. BlockingQueue<E> — заготовка остальных шагов.
Это наши классы, не java.util.concurrent.BlockingQueue.

ReentrantLock — стандартный мьютекс; lock/unlock — захват/освобождение.
Захват сопровождайте unlock в finally. Semaphore.acquire ждёт разрешение,
release возвращает его. Не ожидайте семафор, удерживая мьютекс.
Готовую блокирующую очередь внутри реализации использовать нельзя.

ЧАСТЬ 1. ОЧЕРЕДЬ БЕЗ ОЖИДАНИЯ — TryQueue
Реализуйте offer и poll под одним мьютексом. offer возвращает false при полной
очереди; poll возвращает null при пустой. Методы не ждут появления места/данных,
но захват мьютекса пока может ожидать. Не читайте size вне защиты.
Используйте addLast/removeFirst, чтобы сохранить FIFO. Предел задаётся полем capacity.
Запустите try. Реализацию ArrayDeque менять или писать заново не нужно.

ЧАСТЬ 2. БЛОКИРУЮЩАЯ ОЧЕРЕДЬ — BlockingQueue.put/take
В конструкторе уже созданы два семафора: slots с capacity разрешениями и items с нулём.
Производитель сначала резервирует место, затем под мьютексом вставляет элемент,
после успешной вставки публикует разрешение для потребителя. Потребитель делает
симметричные действия. Семафор, разрешающий следующую операцию, пополняется
только после успешного изменения деки. Мьютекс не удерживается во время
ожидания семафора. Проверку null выполняйте ДО получения разрешений.
Сразу используйте finally для освобождения мьютекса; полную обработку отмены
доведёте в части 4. Запустите blocking и demo.


ЧАСТЬ 3. ПРОИЗВОДИТЕЛИ И ПОТРЕБИТЕЛИ
Запустите transfer. В готовом сценарии производители посылают уникальные Job;
потребители отмечают номера в AtomicIntegerArray, обнаруживая потери и дубли.
Координатор ждёт окончания производителей, кладёт по одному Job(-1) на потребителя
и ждёт потребителей. Код сценария уже дан: ваша задача обеспечить его работу.
Маркер — обычный элемент: занимает место, сохраняет FIFO, извлекается один раз.

*/
public class BoundedQueueWorkshop {
    static final class TryQueue<E> {
        private final ArrayDeque<E> buffer = new ArrayDeque<>();
        private final int capacity;
        private final ReentrantLock mutex = new ReentrantLock();
        TryQueue(int capacity) {
            if (capacity <= 0) throw new IllegalArgumentException("capacity must be positive");
            this.capacity = capacity;
        }
        boolean offer(E value) {
            if (value == null) throw new NullPointerException();
            mutex.lock();
            try {
                if (buffer.size() >= capacity) return false;
                buffer.addLast(value);
                return true;
            } finally { mutex.unlock(); }
        }
        E poll() {
            mutex.lock();
            try {
                return buffer.isEmpty() ? null : buffer.removeFirst();
            } finally { mutex.unlock(); }
        }
    }

    static final class BlockingQueue<E> {
        private final ArrayDeque<E> buffer = new ArrayDeque<>();
        private final int capacity;
        private final ReentrantLock mutex = new ReentrantLock();
        private final Semaphore slots;
        private final Semaphore items;
        BlockingQueue(int capacity) {
            if (capacity <= 0) throw new IllegalArgumentException("capacity must be positive");
            this.capacity = capacity;
            slots = new Semaphore(capacity);
            items = new Semaphore(0);
        }
        void put(E value) throws InterruptedException {
            if (value == null) throw new NullPointerException();
            slots.acquire();              // резервируем место
            mutex.lock();
            try {
                buffer.addLast(value);
            } finally { mutex.unlock(); }
            items.release();              // сообщаем потребителям
        }
        E take() throws InterruptedException {
            items.acquire();              // резервируем элемент
            E value;
            mutex.lock();
            try {
                value = buffer.removeFirst();
            } finally { mutex.unlock(); }
            slots.release();              // освобождаем место
            return value;
        }

    }

    record Job(int id) { boolean isStop() { return id == -1; } }
    private static UnsupportedOperationException todo(int step) {
        return new UnsupportedOperationException("Выполните часть " + step);
    }

    // ===== Готовая обвязка. Решения задания ниже нет. =====
    private static final long DEADLINE_SECONDS = 15;
    @FunctionalInterface interface Action { void run() throws Exception; }

    static final class Group {
        final AtomicReference<Throwable> failure;
        Group() { this(new AtomicReference<>()); }
        Group(AtomicReference<Throwable> failure) { this.failure = failure; }
        final List<Thread> threads = new ArrayList<>();
        Thread start(String name, Action action) {
            Thread t = new Thread(() -> {
                try { action.run(); }
                catch (Throwable e) { failure.compareAndSet(null, e); }
            }, name);
            t.setDaemon(true);
            threads.add(t);
            t.start();
            return t;
        }
        void join() throws Exception { join(DEADLINE_SECONDS); }
        void join(long seconds) throws Exception {
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(seconds);
            for (Thread t : threads) {
                while (t.isAlive()) {
                    checkFailure();
                    if (System.nanoTime() >= deadline) throw new AssertionError("Таймаут: " + t.getName());
                    t.join(20);
                }
            }
            checkFailure();
        }
        void checkFailure() {
            if (failure.get() != null) throw new AssertionError("Ошибка рабочего потока", failure.get());
        }
    }
    private static void require(boolean condition, String message) {
        if (!condition) throw new AssertionError(message);
    }
    private static void await(CountDownLatch latch) throws InterruptedException {
        require(latch.await(DEADLINE_SECONDS, TimeUnit.SECONDS), "Таймаут сигнала");
    }
    // Только в покое: чтение внутренних полей здесь не заменяет рабочую синхронизацию.
    private static void idle(BlockingQueue<?> q, int size) {
        require(q.buffer.size() == size, "Неверный размер");
        require(q.items.availablePermits() == size, "Потеря/избыток разрешений items");
        require(q.slots.availablePermits() == q.capacity - size, "Потеря/избыток slots");
    }

    private static void dequeTest() {
        ArrayDeque<Integer> deque = new ArrayDeque<>();
        for (int i = 0; i < 100; i++) {
            deque.addLast(i); deque.addLast(i + 1);
            require(deque.removeFirst() == i && deque.removeFirst() == i + 1, "FIFO деки");
            require(deque.isEmpty(), "Дека должна быть пуста");
        }
        System.out.println("OK: готовая ArrayDeque, добавление в конец и извлечение из начала");
    }
    private static void tryTest() throws Exception {
        TryQueue<Integer> q = new TryQueue<>(2);
        require(q.poll() == null, "Пустая очередь");
        require(q.offer(1) && q.offer(2) && !q.offer(3), "Переполнение");
        require(q.poll() == 1 && q.offer(3) && q.poll() == 2 && q.poll() == 3, "FIFO");
        require(q.poll() == null, "Очередь должна быть пуста");
        boolean rejected = false;
        try { q.offer(null); } catch (NullPointerException expected) { rejected = true; }
        require(rejected, "null запрещён в TryQueue");
        TryQueue<Integer> concurrent = new TryQueue<>(3);
        AtomicIntegerArray seen = new AtomicIntegerArray(4000);
        Group group = new Group();
        for (int producer = 0; producer < 2; producer++) {
            final int base = producer * 2000;
            group.start("try-producer-" + producer, () -> {
                for (int i = 0; i < 2000; i++)
                    while (!concurrent.offer(base + i)) { group.checkFailure(); Thread.yield(); }
            });
        }
        group.start("try-consumer", () -> {
            for (int i = 0; i < 4000; i++) {
                Integer value;
                while ((value = concurrent.poll()) == null) { group.checkFailure(); Thread.yield(); }
                require(value >= 0 && value < 4000, "Чужой номер в TryQueue");
                require(seen.incrementAndGet(value) == 1, "Дубль в TryQueue");
            }
        });
        group.join();
        for (int i = 0; i < 4000; i++) require(seen.get(i) == 1, "Потеря в TryQueue");
        require(concurrent.poll() == null, "Лишние элементы TryQueue");
        // Циклы опроса выше — нагрузка для offer/poll, не реализация put/take.
        System.out.println("OK: FIFO и 4000 конкурентных сообщений TryQueue");
    }
    private static void blockingTest() throws Exception {
        BlockingQueue<Integer> q = new BlockingQueue<>(1);
        q.put(1);
        Group g = new Group();
        CountDownLatch attempting = new CountDownLatch(1), completed = new CountDownLatch(1);
        g.start("put-full", () -> { attempting.countDown(); q.put(2); completed.countDown(); });
        await(attempting);
        require(!completed.await(60, TimeUnit.MILLISECONDS), "put завершился в полной очереди");
        require(q.take() == 1, "Первый элемент");
        await(completed); g.join();
        require(q.take() == 2, "Второй элемент"); idle(q, 0);
        CountDownLatch got = new CountDownLatch(1);
        Group consumer = new Group();
        consumer.start("take-empty", () -> { require(q.take() == 3, "take"); got.countDown(); });
        require(!got.await(60, TimeUnit.MILLISECONDS), "take завершился в пустой очереди");
        q.put(3); consumer.join(); idle(q, 0);
        System.out.println("OK: ожидание места и элемента");
    }
    private static void transfer(int capacity, int producerCount, int consumerCount, int each) throws Exception {
        require(capacity > 0 && producerCount > 0 && consumerCount > 0 && each > 0, "Параметры должны быть > 0");
        int total = Math.multiplyExact(producerCount, each);
        BlockingQueue<Job> q = new BlockingQueue<>(capacity);
        AtomicIntegerArray seen = new AtomicIntegerArray(total);
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Group producers = new Group(failure), consumers = new Group(failure);
        long start = System.nanoTime();
        for (int c = 0; c < consumerCount; c++) {
            consumers.start("consumer-" + c, () -> {
                while (true) {
                    Job job = q.take();
                    if (job.isStop()) return;
                    require(job.id >= 0 && job.id < total, "Чужой номер");
                    require(seen.incrementAndGet(job.id) == 1, "Дубль: " + job.id);
                }
            });
        }
        for (int p = 0; p < producerCount; p++) {
            final int base = p * each;
            producers.start("producer-" + p, () -> {
                for (int i = 0; i < each; i++) q.put(new Job(base + i));
            });
        }
        producers.join(); consumers.checkFailure();
        for (int c = 0; c < consumerCount; c++) q.put(new Job(-1));
        consumers.join();
        for (int i = 0; i < total; i++) require(seen.get(i) == 1, "Потеря: " + i);
        idle(q, 0);
        System.out.printf("OK: %d сообщений без потерь/дублей; capacity=%d; %.1f ms%n",
                total, capacity, (System.nanoTime() - start) / 1e6);
    }
    private static void demo(int capacity, long delayMillis) throws Exception {
        require(capacity > 0 && delayMillis >= 0 && delayMillis <= 1000, "capacity>0, delay=0..1000");
        BlockingQueue<Job> q = new BlockingQueue<>(capacity);
        Group consumers = new Group();
        consumers.start("slow-consumer", () -> {
            while (true) {
                Job job = q.take();
                if (job.isStop()) return;
                Thread.sleep(delayMillis);
                System.out.println("Обработан Job " + job.id);
            }
        });
        for (int i = 0; i < 8; i++) {
            long start = System.nanoTime();
            q.put(new Job(i));
            System.out.printf("put(%d) вернулся через %.1f ms%n", i, (System.nanoTime()-start)/1e6);
        }
        q.put(new Job(-1)); consumers.join(); idle(q, 0);
        System.out.println("OK: демонстрация завершена");
    }
    private static void run(String command, String[] args) throws Exception {
        switch (command) {
            case "deque" -> dequeTest();
            case "try" -> tryTest();
            case "blocking" -> blockingTest();
            case "transfer" -> transfer(args.length > 1 ? Integer.parseInt(args[1]) : 3,
                    args.length > 2 ? Integer.parseInt(args[2]) : 4,
                    args.length > 3 ? Integer.parseInt(args[3]) : 3,
                    args.length > 4 ? Integer.parseInt(args[4]) : 2000);
            case "demo" -> demo(args.length > 1 ? Integer.parseInt(args[1]) : 1,
                    args.length > 2 ? Long.parseLong(args[2]) : 150);
            default -> throw new IllegalArgumentException(command);
        }
    }
    public static void main(String[] args) throws Exception {
        String command = args.length == 0 ? "help" : args[0];
        if (command.equals("help")) {
            System.out.println("Читайте задание в начале файла; реализуйте TODO по частям.");
            System.out.println("java BoundedQueueWorkshop.java deque|try|blocking|demo|transfer|all");
            return;
        }
        // Обёртка защищает также main от зависания при неверной реализации очереди.
        Group runner = new Group();
        runner.start("scenario", () -> {
            if (command.equals("all")) {
                for (String step : new String[] {"deque", "try", "blocking", "transfer"}) {
                    System.out.println("=== " + step + " ===");
                    run(step, new String[] {step});
                }
            } else run(command, args);
        });
        runner.join(command.equals("all") ? 90 : 30);
    }
}
