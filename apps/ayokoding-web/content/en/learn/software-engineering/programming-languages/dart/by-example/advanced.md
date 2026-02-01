---
title: "Advanced"
weight: 10000003
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Examples 51-75: isolates, design patterns, testing, performance optimization, production patterns, clean architecture (75-95% coverage) - 25 advanced examples"
tags: ["dart", "advanced", "isolates", "testing", "performance", "clean-architecture", "by-example"]
---

Master advanced Dart patterns through 25 heavily annotated examples using Islamic finance contexts. Each example maintains 1-2.25 annotation density and demonstrates sophisticated patterns for production-grade applications.

## Examples 51-60: Advanced Async and Isolates

### Example 51: Isolates Basics

Parallel execution with isolates for CPU-intensive work without blocking main thread.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
sequenceDiagram
    participant Main as Main Isolate<br/>Memory Space 1
    participant Worker as Worker Isolate<br/>Memory Space 2

    Main->>Main: Create ReceivePort
    Main->>Worker: Isolate.spawn(function, sendPort)
    Note over Worker: Separate memory<br/>True parallelism

    Worker->>Main: Send SendPort
    Note over Main,Worker: Bi-directional<br/>communication established

    Main->>Worker: Send data via SendPort
    Worker->>Worker: Process data<br/>(CPU-intensive work)
    Worker->>Main: Send result via SendPort

    Main->>Main: Receive result
    Main->>Worker: Kill isolate

    Note over Main,Worker: No shared memory<br/>Message passing only
```

```dart
import 'dart:isolate';                  // => Import for Isolate

// Function to run in isolate
void heavyCalculation(SendPort sendPort) {
                                        // => Top-level or static function only
                                        // => sendPort sends results back
  double total = 0.0;                   // => Accumulator

  for (int i = 0; i < 100000000; i++) {  // => CPU-intensive loop
    total += i * 0.025;                 // => Simulate Zakat calculation
  }                                     // => Loops complete

  sendPort.send(total);                 // => Send result to main isolate
}                                       // => Isolate exits

void main() async {
  print('Starting calculation...');     // => Immediate output

  // Create receive port
  ReceivePort receivePort = ReceivePort();
                                        // => Port to receive messages
  SendPort sendPort = receivePort.sendPort;
                                        // => Get send port

  // Spawn isolate
  await Isolate.spawn(heavyCalculation, sendPort);
                                        // => Start new isolate
                                        // => Runs heavyCalculation in parallel
                                        // => Main thread continues immediately

  print('Isolate spawned, main thread free');
                                        // => Output immediately (non-blocking)

  // Wait for result
  double result = await receivePort.first as double;
                                        // => Listen for first message
                                        // => Waits until isolate sends result
  print('Result: $result');             // => Output result

  print('Main thread continues');       // => After isolate completes
}
```

**Key Takeaway**: Isolates provide true parallelism (separate memory, no shared state). Use for CPU-intensive work. Communicate via SendPort/ReceivePort. Only top-level/static functions can run in isolates.

**Expected Output**:

```
Starting calculation...
Isolate spawned, main thread free
Result: 124999998750000000.0
Main thread continues
```

**Common Pitfalls**: Can't share objects between isolates. Only send/receive primitive types or special types. Heavy isolate spawn cost - reuse for multiple tasks.

### Example 52: Isolate Communication

Bidirectional communication between isolates with multiple messages.

```dart
import 'dart:isolate';

// Isolate function that processes multiple messages
void zakatWorker(SendPort mainSendPort) {
                                        // => Worker isolate
  ReceivePort workerPort = ReceivePort();
                                        // => Port to receive work

  mainSendPort.send(workerPort.sendPort);
                                        // => Send worker's SendPort to main

  workerPort.listen((message) {         // => Listen for messages
    if (message is Map<String, dynamic>) {
      double wealth = message['wealth'] as double;
      double zakat = wealth * 0.025;    // => Calculate Zakat

      mainSendPort.send({               // => Send result back
        'wealth': wealth,
        'zakat': zakat,
      });
    } else if (message == 'exit') {     // => Exit signal
      Isolate.exit();                   // => Kill isolate
    }
  });
}

void main() async {
  ReceivePort mainPort = ReceivePort();  // => Main's receive port
  await Isolate.spawn(zakatWorker, mainPort.sendPort);
                                        // => Spawn worker

  // Get worker's SendPort
  SendPort workerSendPort = await mainPort.first as SendPort;
                                        // => Receive worker's port

  // Send multiple tasks
  List<double> wealthValues = [100000000.0, 150000000.0, 200000000.0];

  for (double wealth in wealthValues) {  // => Send each value
    workerSendPort.send({'wealth': wealth});
                                        // => Send to worker
  }

  // Receive results
  int received = 0;                     // => Counter
  await for (var message in mainPort) {  // => Listen for results
    if (message is Map<String, dynamic>) {
      print('Wealth: Rp${message['wealth']}, Zakat: Rp${message['zakat']}');
      received++;

      if (received == wealthValues.length) {
                                        // => All results received
        workerSendPort.send('exit');    // => Signal worker to exit
        break;                          // => Stop listening
      }
    }
  }

  print('All calculations complete');
}
```

**Key Takeaway**: Bidirectional communication via SendPort exchange. Worker isolate listens continuously. Send exit signal to terminate isolate. Useful for worker pool patterns.

**Expected Output**:

```
Wealth: Rp100000000.0, Zakat: Rp2500000.0
Wealth: Rp150000000.0, Zakat: Rp3750000.0
Wealth: Rp200000000.0, Zakat: Rp5000000.0
All calculations complete
```

**Common Pitfalls**: SendPort not serializable - exchange via initial message. Isolates don't auto-terminate - send exit signal. Message order not guaranteed.

### Example 53: Compute Function

Simplified isolate usage with compute() helper for single computations.

```dart
import 'dart:isolate';                  // => Import for compute
import 'package:flutter/foundation.dart' show compute;
                                        // => compute from Flutter foundation
                                        // => (For CLI, use Isolate.run in Dart 2.19+)

// Pure function for compute
double calculateTotalZakat(List<double> wealthList) {
                                        // => Must be top-level or static
                                        // => Takes single argument
  return wealthList.fold(0.0, (sum, wealth) => sum + (wealth * 0.025));
                                        // => Calculate sum of all Zakats
}

void main() async {                     // => Main async to use await with compute
                                        // => Async enables await keyword
  List<double> wealthValues = [         // => List of wealth values to calculate Zakat for
                                        // => Data will be passed to isolate
    100000000.0,                        // => 100M IDR
    150000000.0,                        // => 150M IDR
    200000000.0,                        // => 200M IDR
    250000000.0,                        // => 250M IDR
  ];                                    // => Test data (total: 700M)

  print('Calculating total Zakat...');  // => Start message

  // Use compute (automatically spawns and manages isolate)
  double totalZakat = await compute(calculateTotalZakat, wealthValues);
                                        // => Runs in isolate
                                        // => Returns result
                                        // => Isolate auto-terminated

  print('Total Zakat: Rp$totalZakat');  // => Output result

  // Alternative: Isolate.run (Dart 2.19+)
  double totalZakat2 = await Isolate.run(() {
                                        // => One-shot isolate
    return wealthValues.fold(0.0, (sum, w) => sum + (w * 0.025));
                                        // => Closure captures wealthValues
  });                                   // => Isolate auto-terminated

  print('Total Zakat (run): Rp$totalZakat2');
}
```

**Key Takeaway**: `compute()` simplifies isolate usage for single computations. Automatically spawns, executes, and terminates isolate. `Isolate.run()` in Dart 2.19+ for closure support.

**Expected Output**:

```
Calculating total Zakat...
Total Zakat: Rp17500000.0
Total Zakat (run): Rp17500000.0
```

**Common Pitfalls**: `compute()` for single tasks only (not continuous processing). Function must be top-level or static. `Isolate.run()` allows closures but requires Dart 2.19+.

### Example 54: Stream.periodic for Recurring Tasks

Creating periodic streams for scheduled operations.

```dart
import 'dart:async';

void main() async {
  print('Starting periodic Zakat reminders...');

  // Create periodic stream
  Stream<int> periodicStream = Stream.periodic(
    Duration(seconds: 1),               // => Emit every 1 second
    (int count) => count + 1,           // => Transform count to value
  );                                    // => Infinite stream

  // Take limited items
  await for (int day in periodicStream.take(5)) {
                                        // => Take first 5 emissions
    print('Day $day: Zakat reminder');  // => Output each day
  }                                     // => Stream completes after 5

  print('Reminders complete\n');

  // Periodic with business logic
  int zakatDue = 30;                    // => Days until Zakat due
  Stream<String> countdown = Stream.periodic(
    Duration(milliseconds: 500),        // => Every 500ms
    (count) {                           // => Transform function
      int remaining = zakatDue - count;  // => Calculate remaining days
      if (remaining <= 0) {             // => Check if reached zero
        return 'Zakat Due NOW!';
      }
      return 'Days remaining: $remaining';
    },
  ).take(32);                           // => Limit to 32 events

  await for (String message in countdown) {
    print(message);                     // => Output each message
    if (message.contains('NOW')) {      // => Check for completion
      break;                            // => Stop early
    }
  }

  // Periodic with timeout
  try {
    await for (int tick in Stream.periodic(Duration(milliseconds: 100), (i) => i).timeout(Duration(seconds: 1))) {
                                        // => Timeout after 1 second
      print('Tick: $tick');
    }
  } on TimeoutException {               // => Catch timeout
    print('Stream timed out');
  }
}
```

**Key Takeaway**: `Stream.periodic()` creates recurring event streams. Useful for polling, reminders, scheduled tasks. Infinite by default - use `take()` to limit. Combine with `timeout()` for safety.

**Expected Output**:

```
Starting periodic Zakat reminders...
Day 1: Zakat reminder
Day 2: Zakat reminder
Day 3: Zakat reminder
Day 4: Zakat reminder
Day 5: Zakat reminder
Reminders complete

Days remaining: 30
Days remaining: 29
...
Days remaining: 1
Zakat Due NOW!
Tick: 0
Tick: 1
...
Tick: 9
Stream timed out
```

**Common Pitfalls**: Periodic streams are infinite - always limit. Transform function receives increasing count. Don't forget `await for` or stream won't execute.

### Example 55: Future.any and Future.race

Racing multiple Futures to get first completion.

```dart
import 'dart:async';

Future<double> fetchFromServer1() async {
  await Future.delayed(Duration(milliseconds: 800));
                                        // => Simulated delay
  return 500000.0;                      // => Server 1 response
}

Future<double> fetchFromServer2() async {
  await Future.delayed(Duration(milliseconds: 500));
                                        // => Faster server
  return 600000.0;                      // => Server 2 response
}

Future<double> fetchFromServer3() async {
  await Future.delayed(Duration(milliseconds: 1000));
                                        // => Slowest server
  return 550000.0;                      // => Server 3 response
}

void main() async {
  print('Fetching from multiple servers...');

  // Future.any - returns first to complete
  Stopwatch stopwatch = Stopwatch()..start();

  double amount = await Future.any([    // => Race multiple Futures
    fetchFromServer1(),                 // => Server 1
    fetchFromServer2(),                 // => Server 2 (fastest)
    fetchFromServer3(),                 // => Server 3
  ]);                                   // => Returns first completion

  stopwatch.stop();

  print('First response: Rp$amount');   // => Output: Rp600000.0 (from server 2)
  print('Time: ${stopwatch.elapsedMilliseconds}ms');
                                        // => ~500ms (fastest server time)

  // Timeout pattern with Future.any
  try {
    double result = await Future.any([
      fetchFromServer3(),               // => Slow operation
      Future.delayed(Duration(milliseconds: 700), () => throw TimeoutException('Too slow')),
                                        // => Timeout Future
    ]);
    print('Result: Rp$result');
  } on TimeoutException catch (e) {     // => Catch timeout
    print('Timeout: $e');               // => Handle timeout case
  }

  // Fallback pattern
  double donationAmount = await Future.any([
    fetchFromServer2(),                 // => Primary source
    Future.delayed(Duration(seconds: 2), () => 0.0),
                                        // => Fallback after delay
  ]);

  print('Amount (with fallback): Rp$donationAmount');
}
```

**Key Takeaway**: `Future.any()` returns first completing Future. Useful for racing multiple sources (servers, caches). Implement timeouts by racing with delayed Future. Other Futures continue running after first completes.

**Expected Output**:

```
Fetching from multiple servers...
First response: Rp600000.0
Time: ~500ms
Timeout: TimeoutException: Too slow
Amount (with fallback): Rp600000.0
```

**Common Pitfalls**: Other Futures continue executing after first completes. `Future.any` propagates first completion (success or error). For timeout, use `Future.timeout()` instead.

## Examples 56-65: Design Patterns and Architecture

### Example 56: Singleton Pattern

Ensuring single instance of class with factory constructor.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
sequenceDiagram
    participant C1 as Client 1
    participant Factory as Factory Constructor
    participant Instance as Single Instance
    participant C2 as Client 2

    C1->>Factory: new Singleton()
    Factory->>Factory: Check _instance
    Note over Factory: _instance is null

    Factory->>Instance: Create instance
    Instance-->>Factory: Return reference
    Factory-->>C1: Return reference

    C2->>Factory: new Singleton()
    Factory->>Factory: Check _instance
    Note over Factory: _instance exists

    Factory-->>C2: Return existing reference

    Note over C1,C2: Both clients share<br/>same instance
```

```dart
class ZakatCalculatorService {          // => Singleton service
  // Private static instance
  static ZakatCalculatorService? _instance;
                                        // => Nullable instance field

  // Private constructor
  ZakatCalculatorService._internal();   // => Named constructor (private)
                                        // => Prevents external instantiation

  // Factory constructor returns singleton
  factory ZakatCalculatorService() {    // => Factory constructor
    _instance ??= ZakatCalculatorService._internal();
                                        // => Create if null
                                        // => ??= assigns only if null
    return _instance!;                  // => Return existing instance
  }

  // Service state
  int _calculationCount = 0;            // => Shared state

  // Service methods
  double calculateZakat(double wealth) {
    _calculationCount++;                // => Increment counter
    return wealth * 0.025;              // => Calculate
  }

  int get calculationCount => _calculationCount;
}

void main() {
  // Get singleton instance
  var service1 = ZakatCalculatorService();
  var service2 = ZakatCalculatorService();

  // Both reference same instance
  print('Same instance: ${identical(service1, service2)}');
                                        // => Output: Same instance: true

  // Shared state
  service1.calculateZakat(100000000.0);  // => Increment count to 1
  service2.calculateZakat(150000000.0);  // => Increment count to 2

  print('Count from service1: ${service1.calculationCount}');
                                        // => Output: Count from service1: 2
  print('Count from service2: ${service2.calculationCount}');
                                        // => Output: Count from service2: 2
                                        // => Same instance, same state
}
```

**Key Takeaway**: Singleton ensures single instance across application. Use factory constructor to control instantiation. Private constructor prevents external creation. Useful for shared services, configs, caches.

**Expected Output**:

```
Same instance: true
Count from service1: 2
Count from service2: 2
```

**Common Pitfalls**: Singletons complicate testing (global state). Consider dependency injection instead. Thread-safe by default in Dart (single-threaded).

### Example 57: Factory Pattern

Creating objects without specifying exact class with factory methods.

```dart
// Abstract payment interface
abstract class Payment {                // => Abstract base class
  void process();                       // => Abstract method
  double get fee;                       // => Abstract getter
}

// Concrete implementations
class CashPayment implements Payment {  // => Cash payment type
  final double amount;

  CashPayment(this.amount);

  @override
  void process() {
    print('Processing cash payment: Rp$amount');
  }

  @override
  double get fee => 0.0;                // => No fee for cash
}

class CardPayment implements Payment {  // => Card payment type
  final double amount;

  CardPayment(this.amount);

  @override
  void process() {
    print('Processing card payment: Rp$amount');
  }

  @override
  double get fee => amount * 0.01;      // => 1% card fee
}

class TransferPayment implements Payment {
  final double amount;

  TransferPayment(this.amount);

  @override
  void process() {
    print('Processing bank transfer: Rp$amount');
  }

  @override
  double get fee => amount * 0.005;     // => 0.5% transfer fee
}

// Factory class
class PaymentFactory {                  // => Factory creates Payment objects
  // Factory method
  static Payment createPayment(String type, double amount) {
                                        // => Factory method
    switch (type) {                     // => Select implementation
      case 'cash':
        return CashPayment(amount);     // => Return cash instance
      case 'card':
        return CardPayment(amount);     // => Return card instance
      case 'transfer':
        return TransferPayment(amount);  // => Return transfer instance
      default:
        throw ArgumentError('Unknown payment type: $type');
    }
  }
}

void main() {
  // Use factory to create payments
  List<String> types = ['cash', 'card', 'transfer'];
  double amount = 500000.0;

  for (String type in types) {          // => Create each type
    Payment payment = PaymentFactory.createPayment(type, amount);
                                        // => Factory returns correct type
    payment.process();                  // => Polymorphic call
    print('Fee: Rp${payment.fee}\n');   // => Access interface
  }

  // Client code doesn't know concrete types
  Payment donation = PaymentFactory.createPayment('card', 1000000.0);
                                        // => Type: Payment (abstract)
  donation.process();                   // => Runtime polymorphism
  print('Total: Rp${1000000.0 + donation.fee}');
}
```

**Key Takeaway**: Factory pattern decouples object creation from usage. Clients depend on abstract interface. Factory method selects concrete class. Useful for complex object creation logic.

**Expected Output**:

```
Processing cash payment: Rp500000.0
Fee: Rp0.0

Processing card payment: Rp500000.0
Fee: Rp5000.0

Processing bank transfer: Rp500000.0
Fee: Rp2500.0

Processing card payment: Rp1000000.0
Total: Rp1010000.0
```

**Common Pitfalls**: Factory can become complex switch statement. Consider using Map<String, Function> for extensibility. Abstract classes can't be instantiated.

### Example 58: Observer Pattern with Streams

Implementing observer pattern using Dart Streams for event notification.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
sequenceDiagram
    participant Subject as DonationTracker<br/>(Subject)
    participant Stream as StreamController
    participant O1 as EmailNotifier<br/>(Observer 1)
    participant O2 as AuditLogger<br/>(Observer 2)
    participant O3 as StatsCalculator<br/>(Observer 3)

    O1->>Stream: listen()
    O2->>Stream: listen()
    O3->>Stream: listen()
    Note over Stream: Multiple listeners<br/>(broadcast stream)

    Subject->>Stream: add(donation)
    Stream->>O1: notify(donation)
    Stream->>O2: notify(donation)
    Stream->>O3: notify(donation)

    par All observers react
        O1->>O1: Send email
        O2->>O2: Log transaction
        O3->>O3: Update statistics
    end

    Note over Subject,O3: Decoupled communication<br/>Subject doesn't know observers
```

```dart
import 'dart:async';

// Subject (observable)
class DonationTracker {                 // => Subject being observed
  final StreamController<Map<String, dynamic>> _controller = StreamController.broadcast();
                                        // => Broadcast for multiple listeners

  Stream<Map<String, dynamic>> get donationStream => _controller.stream;
                                        // => Public stream getter

  void recordDonation(String donor, double amount) {
    print('Recording: $donor - Rp$amount');

    _controller.add({                   // => Emit event
      'donor': donor,
      'amount': amount,
      'timestamp': DateTime.now(),
    });                                 // => Notifies all observers
  }

  void dispose() {
    _controller.close();                // => Clean up stream
  }
}

// Observers
class EmailNotifier {                   // => Observer 1
  void startListening(DonationTracker tracker) {
    tracker.donationStream.listen((donation) {
                                        // => Subscribe to stream
      String donor = donation['donor'] as String;
      double amount = donation['amount'] as double;
      print('  [EMAIL] Sending receipt to $donor for Rp$amount');
    });
  }
}

class AuditLogger {                     // => Observer 2
  void startListening(DonationTracker tracker) {
    tracker.donationStream.listen((donation) {
      String donor = donation['donor'] as String;
      print('  [AUDIT] Logged donation from $donor');
    });
  }
}

class StatisticsCalculator {            // => Observer 3
  double _total = 0.0;

  void startListening(DonationTracker tracker) {
    tracker.donationStream.listen((donation) {
      double amount = donation['amount'] as double;
      _total += amount;
      print('  [STATS] Total donations: Rp$_total');
    });
  }
}

void main() async {
  // Create subject
  DonationTracker tracker = DonationTracker();

  // Create and attach observers
  EmailNotifier emailer = EmailNotifier();
  emailer.startListening(tracker);      // => Observer 1 subscribes

  AuditLogger logger = AuditLogger();
  logger.startListening(tracker);       // => Observer 2 subscribes

  StatisticsCalculator stats = StatisticsCalculator();
  stats.startListening(tracker);        // => Observer 3 subscribes

  // Subject emits events
  tracker.recordDonation('Ahmad', 500000.0);
                                        // => All observers notified
  await Future.delayed(Duration(milliseconds: 100));

  tracker.recordDonation('Fatimah', 1000000.0);
                                        // => All observers notified again
  await Future.delayed(Duration(milliseconds: 100));

  tracker.dispose();                    // => Clean up
}
```

**Key Takeaway**: Use Streams for observer pattern in Dart. StreamController broadcasts to multiple listeners. Observers subscribe with `listen()`. Subject emits with `add()`. More idiomatic than manual observer lists.

**Expected Output**:

```
Recording: Ahmad - Rp500000.0
  [EMAIL] Sending receipt to Ahmad for Rp500000.0
  [AUDIT] Logged donation from Ahmad
  [STATS] Total donations: Rp500000.0
Recording: Fatimah - Rp1000000.0
  [EMAIL] Sending receipt to Fatimah for Rp1000000.0
  [AUDIT] Logged donation from Fatimah
  [STATS] Total donations: Rp1500000.0
```

**Common Pitfalls**: Use broadcast stream for multiple listeners. Remember to close StreamController. Synchronous listeners may block.

## Examples 59-70: Testing and Quality

### Example 59: Unit Testing Basics

Writing unit tests with package:test framework.

```dart
// File: zakat_calculator.dart
class ZakatCalculator {
  static const double rate = 0.025;
  static const double nisabThreshold = 85000000.0;

  double calculateZakat(double wealth) {
    if (wealth < 0) {
      throw ArgumentError('Wealth must be positive');
    }
    if (wealth < nisabThreshold) {
      return 0.0;
    }
    return wealth * rate;
  }

  bool isEligible(double wealth) {
    return wealth >= nisabThreshold;
  }
}

// File: zakat_calculator_test.dart
import 'package:test/test.dart';        // => Test framework

void main() {
  group('ZakatCalculator', () {         // => Group related tests
    late ZakatCalculator calculator;    // => Test subject

    setUp(() {                          // => Runs before each test
      calculator = ZakatCalculator();   // => Fresh instance
    });

    test('calculates Zakat correctly for eligible wealth', () {
      double result = calculator.calculateZakat(100000000.0);
                                        // => Execute
      expect(result, equals(2500000.0));  // => Assert result
    });

    test('returns zero for wealth below nisab', () {
      double result = calculator.calculateZakat(50000000.0);
      expect(result, equals(0.0));      // => Assert zero
    });

    test('throws ArgumentError for negative wealth', () {
      expect(
        () => calculator.calculateZakat(-1000.0),
                                        // => Closure that throws
        throwsArgumentError,            // => Assert throws
      );
    });

    test('isEligible returns true for wealth above nisab', () {
      bool eligible = calculator.isEligible(100000000.0);
      expect(eligible, isTrue);         // => Assert boolean
    });

    test('isEligible returns false for wealth below nisab', () {
      bool eligible = calculator.isEligible(50000000.0);
      expect(eligible, isFalse);
    });
  });
}
```

**Key Takeaway**: Use `package:test` for unit testing. Organize with `group()`. Use `setUp()` for initialization. `expect()` for assertions. Test happy path, edge cases, and errors.

**Expected Output** (run with `dart test`):

```
00:00 +5: All tests passed!
```

**Common Pitfalls**: Don't share state between tests. Use `setUp()` for fresh instances. Test exceptions with closures. Mock external dependencies.

### Example 60: Integration Testing

Testing multiple components together with async operations.

```dart
import 'package:test/test.dart';
import 'dart:async';

class DonationRepository {              // => Data layer
  Future<void> saveDonation(String donor, double amount) async {
    await Future.delayed(Duration(milliseconds: 100));
                                        // => Simulate database write
  }

  Future<double> getTotalDonations() async {
    await Future.delayed(Duration(milliseconds: 50));
    return 5000000.0;                   // => Mock total
  }
}

class DonationService {                 // => Business logic layer
  final DonationRepository repository;

  DonationService(this.repository);

  Future<bool> processDonation(String donor, double amount) async {
    if (amount <= 0) {
      return false;                     // => Invalid amount
    }

    await repository.saveDonation(donor, amount);
    return true;                        // => Success
  }

  Future<double> calculateTotalWithZakat() async {
    double total = await repository.getTotalDonations();
    double zakat = total * 0.025;
    return total + zakat;
  }
}

void main() {
  group('DonationService Integration', () {
    late DonationRepository repository;
    late DonationService service;

    setUp(() {
      repository = DonationRepository();  // => Real repository
      service = DonationService(repository);
    });

    test('processDonation saves valid donation', () async {
                                        // => async test
      bool result = await service.processDonation('Ahmad', 500000.0);
                                        // => await async operation
      expect(result, isTrue);           // => Assert success
    });

    test('processDonation rejects invalid amount', () async {
      bool result = await service.processDonation('Ahmad', -100.0);
      expect(result, isFalse);          // => Assert rejection
    });

    test('calculateTotalWithZakat includes Zakat', () async {
      double total = await service.calculateTotalWithZakat();
                                        // => Async calculation
      expect(total, equals(5125000.0)); // => 5M + 2.5%
    });

    test('multiple operations complete in sequence', () async {
      await service.processDonation('Ahmad', 500000.0);
      await service.processDonation('Fatimah', 1000000.0);
      double total = await service.calculateTotalWithZakat();

      expect(total, greaterThan(0.0));  // => Assert positive
    });
  });
}
```

**Key Takeaway**: Integration tests verify component interactions. Mark async tests with `async`. Use `await` for async operations. Test realistic workflows. Consider using test doubles for external dependencies.

**Expected Output**:

```
00:00 +4: All tests passed!
```

**Common Pitfalls**: Integration tests slower than unit tests. Don't test implementation details. Use real dependencies where practical, mocks for expensive operations.

## Examples 61-70: Performance and Production Patterns

### Example 61: Lazy Initialization

Deferring expensive initialization until first use.

```dart
class ExpensiveResource {               // => Resource to lazy-load
  ExpensiveResource() {
    print('  Creating expensive resource...');
                                        // => Expensive operation
  }

  void use() {
    print('  Using resource');
  }
}

class ZakatService {
  ExpensiveResource? _resource;         // => Nullable field

  // Lazy getter
  ExpensiveResource get resource {      // => Getter
    _resource ??= ExpensiveResource();  // => Create on first access
                                        // => ??= assigns only if null
    return _resource!;                  // => Return (non-null after assignment)
  }

  void processZakat() {
    print('Processing...');
    resource.use();                     // => First access triggers creation
  }
}

void main() {
  print('Creating service...');
  ZakatService service = ZakatService();  // => Resource not created yet
  print('Service created\n');

  print('First call:');
  service.processZakat();               // => Triggers lazy initialization
  print('');

  print('Second call:');
  service.processZakat();               // => Reuses existing resource
  print('');

  // Alternative: late keyword
  print('Using late keyword:');
  late ExpensiveResource lateResource;  // => late defers initialization
                                        // => Throws if accessed before assignment

  print('Before initialization');
  lateResource = ExpensiveResource();   // => Initialize explicitly
  lateResource.use();                   // => Use after initialization
}
```

**Key Takeaway**: Lazy initialization delays expensive operations until needed. Use nullable field with `??=` operator. Alternative: `late` keyword for non-nullable late initialization. Improves startup performance.

**Expected Output**:

```
Creating service...
Service created

First call:
Processing...
  Creating expensive resource...
  Using resource

Second call:
Processing...
  Using resource

Using late keyword:
Before initialization
  Creating expensive resource...
  Using resource
```

**Common Pitfalls**: `late` without initialization throws at runtime. Lazy initialization not thread-safe (not issue in single-threaded Dart). Consider caching strategy for memory constraints.

---

### Example 62: Memoization and Caching

Caching expensive function results to avoid redundant computation. Critical for performance optimization in production applications.

```dart
class ZakatCalculator {
  final Map<double, double> _cache = {}; // => Cache: stores computed results
                                        // => Key: wealth amount
                                        // => Value: calculated Zakat

  // Expensive calculation (simulated)
  double _expensiveCalculation(double wealth) {
                                        // => Simulate: CPU-intensive work
    int iterations = 1000000;           // => Work: many iterations
    double result = wealth;
    for (int i = 0; i < iterations; i++) {
      result = result * 1.0000001;      // => Simulate: complex calculation
    }
    return wealth * 0.025;              // => Return: Zakat amount
  }

  // Memoized calculation
  double calculate(double wealth) {     // => Public method: with caching
    if (_cache.containsKey(wealth)) {   // => Check: cached result exists
      print('Cache hit for $wealth');
      return _cache[wealth]!;           // => Return: cached value (instant)
    }

    print('Cache miss for $wealth - computing...');
    double result = _expensiveCalculation(wealth);
                                        // => Compute: expensive operation
    _cache[wealth] = result;            // => Store: in cache for future use
    return result;                      // => Return: computed result
  }

  void clearCache() {                   // => Cache management: clear old data
    _cache.clear();
    print('Cache cleared');
  }

  int get cacheSize => _cache.length;   // => Monitoring: cache utilization
}

// Memoization with function wrapper
typedef ExpensiveFunction = double Function(double);

class Memoizer {
  final Map<double, double> _cache = {};
  final ExpensiveFunction _function;    // => Function to memoize

  Memoizer(this._function);

  double call(double input) {           // => Callable class: function-like syntax
    return _cache.putIfAbsent(input, () => _function(input));
                                        // => putIfAbsent: compute only if missing
                                        // => Atomic: insert if absent
  }
}

void main() {
  ZakatCalculator calculator = ZakatCalculator();

  // First calculation (cache miss)
  Stopwatch sw1 = Stopwatch()..start();
  double zakat1 = calculator.calculate(10000000.0);
  sw1.stop();
  print('Zakat: Rp$zakat1 (${sw1.elapsedMilliseconds}ms)\n');

  // Second calculation (cache hit)
  Stopwatch sw2 = Stopwatch()..start();
  double zakat2 = calculator.calculate(10000000.0);
  sw2.stop();
  print('Zakat: Rp$zakat2 (${sw2.elapsedMilliseconds}ms)\n');
                                        // => Near-instant: cached result

  // Different input (cache miss)
  double zakat3 = calculator.calculate(5000000.0);
  print('Zakat: Rp$zakat3\n');

  print('Cache size: ${calculator.cacheSize}');
                                        // => Output: 2 entries

  // Function wrapper memoization
  Memoizer memoized = Memoizer((wealth) => wealth * 0.025);

  print('\nMemoized wrapper:');
  print('First call: ${memoized(8000000.0)}');
                                        // => Computes
  print('Second call: ${memoized(8000000.0)}');
                                        // => Cached

  // Cache eviction strategy (LRU simulation)
  calculator.clearCache();              // => Manual eviction
}
```

**Key Takeaway**: Use memoization to cache expensive function results. Check cache before computing. Use `Map` for key-value caching. Implement cache eviction for memory management. Memoization trades memory for speed.

**Why It Matters**: Memoization prevents redundant expensive calculations, critical for responsive UIs and efficient APIs. In Flutter, memoization caches widget builds, layout calculations, and image processing results. For server applications, caching API responses and database query results reduces latency and server load. Proper cache eviction (LRU, TTL) prevents memory exhaustion.

**Common Pitfalls**: Unbounded cache grows indefinitely (implement size limit or TTL). Cache invalidation complexity (when to clear stale data). Thread-safe caching requires locks in multi-threaded contexts (not needed in Dart isolates). Reference equality vs value equality for cache keys.

---

### Example 63: Memory Management and Object Pooling

Managing memory efficiently through object reuse and explicit cleanup. Reduces garbage collection pressure and allocation overhead.

```dart
// Object pool pattern
class DonationRecord {
  String donor = '';                    // => Mutable: for reuse
  double amount = 0.0;
  DateTime? timestamp;

  void reset() {                        // => Reset: prepare for reuse
    donor = '';
    amount = 0.0;
    timestamp = null;
  }

  void configure(String d, double a, DateTime t) {
                                        // => Configure: set values
    donor = d;
    amount = a;
    timestamp = t;
  }
}

class DonationPool {
  final List<DonationRecord> _available = [];
                                        // => Available: pool of reusable objects
  final List<DonationRecord> _inUse = [];
                                        // => In-use: currently borrowed objects
  final int maxPoolSize;                // => Limit: prevent unbounded growth

  DonationPool({this.maxPoolSize = 10});

  DonationRecord acquire() {            // => Acquire: get object from pool
    if (_available.isEmpty) {           // => Pool empty: create new object
      if (_inUse.length < maxPoolSize) {
        print('Creating new object (pool size: ${_inUse.length + 1})');
        return DonationRecord();
      }
      throw StateError('Pool exhausted');
                                        // => Error: no available objects
    }

    DonationRecord record = _available.removeLast();
                                        // => Reuse: from available pool
    _inUse.add(record);                 // => Track: currently in use
    print('Reusing object (available: ${_available.length})');
    return record;
  }

  void release(DonationRecord record) { // => Release: return to pool
    record.reset();                     // => Clean: prepare for reuse
    _inUse.remove(record);              // => Remove: from in-use list
    _available.add(record);             // => Return: to available pool
    print('Released object (available: ${_available.length})');
  }

  void dispose() {                      // => Cleanup: destroy pool
    _available.clear();
    _inUse.clear();
    print('Pool disposed');
  }
}

// Weak reference simulation (Dart doesn't have WeakReference for objects)
class Cache<K, V> {
  final Map<K, V> _cache = {};
  final Map<K, DateTime> _timestamps = {};
                                        // => TTL: time-to-live for entries
  final Duration ttl;

  Cache({this.ttl = const Duration(minutes: 5)});

  void put(K key, V value) {
    _cache[key] = value;
    _timestamps[key] = DateTime.now();  // => Track: insertion time
  }

  V? get(K key) {
    if (!_cache.containsKey(key)) {
      return null;                      // => Not found
    }

    DateTime? insertTime = _timestamps[key];
    if (insertTime != null && DateTime.now().difference(insertTime) > ttl) {
      _cache.remove(key);               // => Expired: remove stale entry
      _timestamps.remove(key);
      print('Cache entry expired: $key');
      return null;
    }

    return _cache[key];                 // => Return: valid cached value
  }

  void evictExpired() {                 // => Cleanup: remove all expired entries
    DateTime now = DateTime.now();
    List<K> expired = [];

    _timestamps.forEach((key, time) {
      if (now.difference(time) > ttl) {
        expired.add(key);               // => Collect: expired keys
      }
    });

    for (K key in expired) {
      _cache.remove(key);
      _timestamps.remove(key);
    }

    print('Evicted ${expired.length} expired entries');
  }

  int get size => _cache.length;
}

void main() {
  // Object pooling
  DonationPool pool = DonationPool(maxPoolSize: 3);

  // Acquire objects
  DonationRecord rec1 = pool.acquire(); // => Create new
  rec1.configure('Ahmad', 100000.0, DateTime.now());

  DonationRecord rec2 = pool.acquire(); // => Create new
  rec2.configure('Fatimah', 200000.0, DateTime.now());

  // Release objects
  pool.release(rec1);                   // => Return to pool
  pool.release(rec2);

  // Reuse objects
  DonationRecord rec3 = pool.acquire(); // => Reuse rec2
  rec3.configure('Ali', 150000.0, DateTime.now());

  DonationRecord rec4 = pool.acquire(); // => Reuse rec1
  rec4.configure('Umar', 175000.0, DateTime.now());

  pool.release(rec3);
  pool.release(rec4);

  pool.dispose();                       // => Cleanup

  // TTL cache
  print('\nTTL Cache:');
  Cache<String, double> cache = Cache(ttl: Duration(seconds: 2));

  cache.put('donor1', 100000.0);
  print('Cached: ${cache.get('donor1')}');

  // Wait for expiration
  Future.delayed(Duration(seconds: 3), () {
    print('After expiration: ${cache.get('donor1')}');
                                        // => null: expired

    cache.put('donor2', 200000.0);
    cache.put('donor3', 150000.0);
    print('Cache size: ${cache.size}');

    cache.evictExpired();               // => Manual eviction
  });
}
```

**Key Takeaway**: Use object pooling to reuse expensive objects and reduce garbage collection. Implement TTL caching for automatic expiration. Track object lifecycle (acquire/release). Pool size limits prevent memory exhaustion.

**Why It Matters**: Object pooling reduces allocation overhead and garbage collection pauses, critical for real-time applications and high-throughput systems. In Flutter, pooling reusable widgets, animation controllers, and image buffers improves frame rates. For servers, pooling database connections and HTTP clients reduces latency. TTL caching prevents stale data without manual invalidation.

**Common Pitfalls**: Forgetting to reset objects before reuse corrupts state. Pool exhaustion without proper error handling. Memory leaks from objects never released. TTL too short causes cache thrashing, too long wastes memory.

---

### Example 64: String Optimization Techniques

Optimizing string operations for performance in string-heavy applications. Strings are immutable in Dart—each concatenation creates new object.

```dart
void main() {
  // Inefficient: repeated concatenation
  String inefficientConcat() {
    Stopwatch sw = Stopwatch()..start();
    String result = '';                 // => Empty string
    for (int i = 0; i < 10000; i++) {
      result += 'Donation $i, ';        // => BAD: creates new string each iteration
                                        // => O(n²): quadratic time complexity
    }
    sw.stop();
    print('Inefficient concat: ${sw.elapsedMilliseconds}ms');
    return result;
  }

  // Efficient: StringBuffer
  String efficientConcat() {
    Stopwatch sw = Stopwatch()..start();
    StringBuffer buffer = StringBuffer(); // => Mutable: efficient for building strings
    for (int i = 0; i < 10000; i++) {
      buffer.write('Donation $i, ');    // => GOOD: modifies buffer in-place
                                        // => O(n): linear time complexity
    }
    sw.stop();
    print('Efficient concat: ${sw.elapsedMilliseconds}ms');
    return buffer.toString();           // => Convert: final string
  }

  inefficientConcat();                  // => Slow: ~200ms
  efficientConcat();                    // => Fast: ~2ms

  // String interpolation vs concatenation
  String donor = 'Ahmad';
  double amount = 100000.0;

  // Interpolation (efficient, readable)
  String message1 = 'Donor: $donor, Amount: Rp$amount';
                                        // => Efficient: compiled to concatenation
                                        // => Readable: clear intent

  // Concatenation (less efficient for multiple parts)
  String message2 = 'Donor: ' + donor + ', Amount: Rp' + amount.toString();
                                        // => Multiple string objects created

  // String join for lists
  List<String> donors = ['Ahmad', 'Fatimah', 'Ali', 'Umar'];

  // Inefficient
  String list1 = '';
  for (String d in donors) {
    list1 += d + ', ';                  // => Bad: repeated concatenation
  }

  // Efficient
  String list2 = donors.join(', ');     // => Good: single operation
  print('Donors: $list2');

  // Const strings (compile-time constants)
  const String template = 'Zakat due: ';
                                        // => const: compile-time constant
                                        // => Reused: same object for all references
  const String category = 'Zakat';

  // String comparison optimization
  String cat1 = 'Zakat';
  String cat2 = 'Zakat';

  // Identity vs equality
  print('cat1 == cat2: ${cat1 == cat2}');
                                        // => true: value equality
  print('identical(cat1, cat2): ${identical(cat1, cat2)}');
                                        // => May be true: string interning

  // Const strings always identical
  print('identical(category, cat1): ${identical(category, cat1)}');
                                        // => true: const strings interned

  // StringBuffer methods
  StringBuffer report = StringBuffer()
    ..write('=== Donation Report ===\n')
    ..write('Total donors: ${donors.length}\n')
    ..writeAll(donors.map((d) => '- $d'), '\n')
                                        // => writeAll: join with separator
    ..write('\nReport complete');

  print(report.toString());

  // Runes for Unicode handling
  String emoji = '🕌';                  // => Emoji: mosque
  print('String length: ${emoji.length}');
                                        // => 2: UTF-16 code units
  print('Runes length: ${emoji.runes.length}');
                                        // => 1: actual Unicode characters

  // Case conversion caching
  String donorName = 'Ahmad ibn Abdullah';
  String upper = donorName.toUpperCase();
                                        // => Creates new string
  String cached = donorName.toUpperCase();
                                        // => May be cached by implementation
}
```

**String Performance Comparison**:

| Operation            | Time (10k items) | Memory Impact | Use Case                  |
| -------------------- | ---------------- | ------------- | ------------------------- |
| += concatenation     | ~200ms           | High (O(n²))  | Avoid for loops           |
| StringBuffer         | ~2ms             | Low (O(n))    | Building strings in loops |
| String interpolation | ~2ms             | Low           | Readable, few parts       |
| join()               | ~1ms             | Low           | Joining lists             |

**Key Takeaway**: Use `StringBuffer` for repeated concatenation in loops. Prefer string interpolation for readability. Use `join()` for lists. Const strings for compile-time constants. Avoid `+=` in loops (quadratic time).

**Why It Matters**: String operations are ubiquitous in applications—logging, formatting, API responses, UI rendering. Inefficient string handling causes performance bottlenecks. In Flutter, excessive string concatenation in build methods triggers frame drops. For servers, inefficient JSON serialization and log formatting waste CPU cycles. StringBuffer provides O(n) performance vs O(n²) for repeated concatenation.

**Common Pitfalls**: Using `+=` in loops (quadratic complexity). Forgetting to convert StringBuffer to String. Unnecessary `toString()` calls. Not using const for static strings.

---

### Example 65: Collection Performance Optimization

Choosing and using collections efficiently for optimal performance and memory usage.

```dart
void main() {
  // List vs Set for uniqueness checks
  List<int> donorIdsList = List.generate(10000, (i) => i);
  Set<int> donorIdsSet = Set.from(donorIdsList);

  // Contains check (List: O(n), Set: O(1))
  Stopwatch sw1 = Stopwatch()..start();
  for (int i = 0; i < 1000; i++) {
    donorIdsList.contains(5000);        // => O(n): linear search
  }
  sw1.stop();
  print('List contains (1000x): ${sw1.elapsedMilliseconds}ms');

  Stopwatch sw2 = Stopwatch()..start();
  for (int i = 0; i < 1000; i++) {
    donorIdsSet.contains(5000);         // => O(1): hash lookup
  }
  sw2.stop();
  print('Set contains (1000x): ${sw2.elapsedMilliseconds}ms');
                                        // => Set: significantly faster

  // Map for fast lookups
  Map<String, double> donationsMap = {
    'DONOR-001': 100000.0,
    'DONOR-002': 200000.0,
    'DONOR-003': 150000.0,
  };

  double? amount = donationsMap['DONOR-002'];
                                        // => O(1): hash lookup
  print('Amount: Rp$amount');

  // Growing lists
  // Bad: repeated grow operations
  List<int> badList = [];               // => Default capacity: small
  Stopwatch sw3 = Stopwatch()..start();
  for (int i = 0; i < 100000; i++) {
    badList.add(i);                     // => May trigger resize/copy
  }
  sw3.stop();
  print('List without capacity: ${sw3.elapsedMilliseconds}ms');

  // Good: preallocate capacity
  List<int> goodList = List.filled(100000, 0);
                                        // => Preallocated: no resizing
  Stopwatch sw4 = Stopwatch()..start();
  for (int i = 0; i < 100000; i++) {
    goodList[i] = i;                    // => Direct assignment: faster
  }
  sw4.stop();
  print('List with capacity: ${sw4.elapsedMilliseconds}ms');

  // Iteration patterns
  List<double> amounts = List.generate(10000, (i) => i * 100.0);

  // For-in loop (readable, efficient)
  double total1 = 0.0;
  Stopwatch sw5 = Stopwatch()..start();
  for (double amount in amounts) {
    total1 += amount;
  }
  sw5.stop();
  print('For-in loop: ${sw5.elapsedMilliseconds}ms, total: Rp$total1');

  // Index loop (when index needed)
  double total2 = 0.0;
  Stopwatch sw6 = Stopwatch()..start();
  for (int i = 0; i < amounts.length; i++) {
    total2 += amounts[i];
  }
  sw6.stop();
  print('Index loop: ${sw6.elapsedMilliseconds}ms, total: Rp$total2');

  // Functional (readable, may be slower for large collections)
  Stopwatch sw7 = Stopwatch()..start();
  double total3 = amounts.reduce((a, b) => a + b);
  sw7.stop();
  print('Reduce: ${sw7.elapsedMilliseconds}ms, total: Rp$total3');

  // Lazy evaluation with where() and map()
  List<double> largeAmounts = amounts
      .where((a) => a > 500000.0)       // => Lazy: not executed yet
      .map((a) => a * 1.1)              // => Lazy: chained transformation
      .toList();                        // => Execute: materialize results
  print('Large amounts (filtered): ${largeAmounts.length}');

  // Avoid creating intermediate lists
  // Bad: multiple intermediate lists
  List<double> bad = amounts
      .where((a) => a > 500000.0).toList()
                                        // => Creates list
      .map((a) => a * 1.1).toList()     // => Creates another list
      .take(10).toList();               // => Creates third list

  // Good: single materialization
  List<double> good = amounts
      .where((a) => a > 500000.0)       // => Lazy
      .map((a) => a * 1.1)              // => Lazy
      .take(10)                         // => Lazy
      .toList();                        // => Single materialization

  // UnmodifiableListView for safety
  List<double> mutableList = [100000.0, 200000.0];
  List<double> unmodifiable = List.unmodifiable(mutableList);
                                        // => Immutable wrapper
  // unmodifiable.add(300000.0);       // ← Throws UnsupportedError

  print('\nCollection choices:');
  print('- Use Set for uniqueness checks (O(1) contains)');
  print('- Use Map for key-value lookups (O(1) access)');
  print('- Preallocate List capacity for known size');
  print('- Use lazy operations (where, map) without toList()');
  print('- Materialize only final result');
}
```

**Collection Performance**:

| Operation    | List   | Set  | Map  | Use Case          |
| ------------ | ------ | ---- | ---- | ----------------- |
| Index access | O(1)   | N/A  | N/A  | Sequential access |
| contains()   | O(n)   | O(1) | O(1) | Membership check  |
| add()        | O(1)\* | O(1) | O(1) | Insertion         |
| remove()     | O(n)   | O(1) | O(1) | Deletion          |
| Iteration    | Fast   | Fast | Fast | All elements      |

\*Amortized O(1) with occasional resize

**Key Takeaway**: Choose collection type based on access pattern. Use Set for uniqueness and O(1) contains. Use Map for key-value lookups. Preallocate List capacity. Use lazy operations, materialize once.

**Why It Matters**: Wrong collection choice causes performance degradation. List.contains in loops is O(n²), Set.contains is O(1). Repeated List resizing wastes CPU cycles. Eager evaluation creates unnecessary intermediate collections. In Flutter, efficient collections prevent UI lag during scrolling and data updates. For servers, collection choice impacts request latency.

**Common Pitfalls**: Using List when Set appropriate (uniqueness checks). Not preallocating List capacity for known size. Multiple `toList()` calls creating intermediate collections. Forgetting Set/Map have no guaranteed order.

---

### Example 66: Benchmarking and Profiling

Measuring code performance scientifically to identify bottlenecks and validate optimizations.

```dart
import 'dart:async';

// Benchmark harness
class Benchmark {
  final String name;
  final Function operation;             // => Function to benchmark
  final int iterations;                 // => Iterations: for averaging

  Benchmark(this.name, this.operation, {this.iterations = 1000});

  void run() {
    // Warmup phase (JIT compilation)
    for (int i = 0; i < 100; i++) {
      operation();                      // => Warmup: optimize hot code paths
    }

    // Measurement phase
    List<int> timings = [];
    for (int i = 0; i < iterations; i++) {
      Stopwatch sw = Stopwatch()..start();
      operation();
      sw.stop();
      timings.add(sw.elapsedMicroseconds);
    }

    // Statistics
    timings.sort();
    int min = timings.first;
    int max = timings.last;
    double avg = timings.reduce((a, b) => a + b) / timings.length;
    int median = timings[timings.length ~/ 2];
    int p95 = timings[(timings.length * 0.95).floor()];
                                        // => 95th percentile: tail latency

    print('=== $name ===');
    print('Min: ${min}μs');
    print('Max: ${max}μs');
    print('Avg: ${avg.toStringAsFixed(2)}μs');
    print('Median: ${median}μs');
    print('P95: ${p95}μs');
    print('');
  }
}

// Profiling helpers
class Profiler {
  static final Map<String, int> _counts = {};
  static final Map<String, int> _times = {};

  static void start(String operation) {
    _counts[operation] = (_counts[operation] ?? 0) + 1;
  }

  static void record(String operation, int microseconds) {
    _times[operation] = (_times[operation] ?? 0) + microseconds;
  }

  static void printReport() {
    print('=== Profiler Report ===');
    List<String> operations = _times.keys.toList()
      ..sort((a, b) => _times[b]!.compareTo(_times[a]!));
                                        // => Sort: by total time descending

    for (String op in operations) {
      int count = _counts[op] ?? 0;
      int totalTime = _times[op] ?? 0;
      double avgTime = count > 0 ? totalTime / count : 0;

      print('$op:');
      print('  Count: $count');
      print('  Total: ${totalTime}μs');
      print('  Avg: ${avgTime.toStringAsFixed(2)}μs');
    }
  }

  static void clear() {
    _counts.clear();
    _times.clear();
  }
}

void main() {
  // Benchmark string concatenation
  Benchmark('String += concatenation', () {
    String result = '';
    for (int i = 0; i < 100; i++) {
      result += 'x';                    // => Inefficient
    }
  }, iterations: 1000).run();

  Benchmark('StringBuffer', () {
    StringBuffer buffer = StringBuffer();
    for (int i = 0; i < 100; i++) {
      buffer.write('x');                // => Efficient
    }
    buffer.toString();
  }, iterations: 1000).run();

  // Benchmark collection operations
  List<int> list = List.generate(1000, (i) => i);
  Set<int> set = Set.from(list);

  Benchmark('List.contains', () {
    list.contains(500);                 // => O(n)
  }, iterations: 10000).run();

  Benchmark('Set.contains', () {
    set.contains(500);                  // => O(1)
  }, iterations: 10000).run();

  // Profiling example
  print('=== Profiling Example ===');
  Profiler.clear();

  for (int i = 0; i < 1000; i++) {
    Stopwatch sw1 = Stopwatch()..start();
    Profiler.start('Zakat calculation');
    double zakat = 10000000.0 * 0.025;
    sw1.stop();
    Profiler.record('Zakat calculation', sw1.elapsedMicroseconds);

    Stopwatch sw2 = Stopwatch()..start();
    Profiler.start('Database query');
    // Simulate database query
    Future.delayed(Duration.zero);      // => Simulate: async operation
    sw2.stop();
    Profiler.record('Database query', sw2.elapsedMicroseconds);
  }

  Profiler.printReport();

  // Memory profiling (manual tracking)
  print('\n=== Memory Tracking ===');
  int initialObjects = 0;
  List<String> objects = [];

  for (int i = 0; i < 10000; i++) {
    objects.add('Donation $i');         // => Track: object creation
  }

  print('Created objects: ${objects.length}');
  print('Approximate memory: ~${objects.length * 20} bytes');
                                        // => Rough estimate: String overhead
}
```

**Benchmarking Best Practices**:

| Practice            | Rationale                          | Example                        |
| ------------------- | ---------------------------------- | ------------------------------ |
| Warmup phase        | JIT compilation optimizes hot code | 100 iterations before measure  |
| Multiple iterations | Reduce noise, average results      | 1000+ iterations               |
| Measure percentiles | Identify tail latency (outliers)   | P50, P95, P99                  |
| Isolate tests       | One operation per benchmark        | Separate benchmarks per method |
| Profile production  | Dev performance differs from prod  | Profile release builds         |

**Key Takeaway**: Use benchmarks to measure performance scientifically. Include warmup phase for JIT optimization. Measure min, max, avg, median, P95. Profile to identify hotspots. Track memory usage.

**Why It Matters**: Performance optimization without measurement is guesswork. Benchmarking validates optimizations (StringBuffer 100x faster than +=). Profiling identifies unexpected bottlenecks (database queries dominating CPU time). Percentiles reveal tail latency that averages hide. In Flutter, profiling identifies jank sources (slow builds, expensive layouts). For servers, profiling optimizes request handling.

**Common Pitfalls**: Skipping warmup phase (JIT not optimized). Too few iterations (noisy results). Measuring only average (misses outliers). Optimizing without profiling (wrong target). Benchmarking debug mode (unrealistic performance).

---

### Example 67: Production Logging Patterns

Implementing structured logging for production observability and debugging.

```dart
enum LogLevel {
  debug,                                // => Development: verbose details
  info,                                 // => Production: normal events
  warning,                              // => Production: potential issues
  error,                                // => Production: failures
  fatal,                                // => Production: critical failures
}

class Logger {
  final String context;                 // => Context: logger category/component
  LogLevel minLevel = LogLevel.info;    // => Threshold: minimum level to log

  Logger(this.context);

  void _log(LogLevel level, String message, [Object? error, StackTrace? stackTrace]) {
    if (level.index < minLevel.index) {
      return;                           // => Skip: below threshold
    }

    String timestamp = DateTime.now().toIso8601String();
    String levelStr = level.toString().split('.').last.toUpperCase();
    String logLine = '[$timestamp] $levelStr [$context] $message';

    if (error != null) {
      logLine += '\nError: $error';     // => Include: error object
    }

    if (stackTrace != null) {
      logLine += '\nStackTrace:\n$stackTrace';
                                        // => Include: stack trace for errors
    }

    // In production: send to logging service (Sentry, CloudWatch, etc.)
    print(logLine);                     // => Development: print to console
  }

  void debug(String message) => _log(LogLevel.debug, message);
  void info(String message) => _log(LogLevel.info, message);
  void warning(String message) => _log(LogLevel.warning, message);
  void error(String message, [Object? error, StackTrace? stackTrace]) =>
      _log(LogLevel.error, message, error, stackTrace);
  void fatal(String message, [Object? error, StackTrace? stackTrace]) =>
      _log(LogLevel.fatal, message, error, stackTrace);
}

// Structured logging with context
class StructuredLogger {
  final String service;

  StructuredLogger(this.service);

  void log(LogLevel level, String message, {Map<String, dynamic>? context}) {
    Map<String, dynamic> logEntry = {
      'timestamp': DateTime.now().toIso8601String(),
      'level': level.toString().split('.').last,
      'service': service,
      'message': message,
    };

    if (context != null) {
      logEntry['context'] = context;    // => Structured: additional metadata
    }

    // JSON format for log aggregation (ELK, Datadog, etc.)
    print(logEntry);                    // => Production: send as JSON
  }
}

void main() {
  // Basic logging
  Logger zakatLogger = Logger('ZakatService');

  zakatLogger.debug('Initializing service');
                                        // => Not logged: below threshold (info)

  zakatLogger.minLevel = LogLevel.debug;
                                        // => Lower threshold: for debugging

  zakatLogger.debug('Debug mode enabled');
                                        // => Now logged

  zakatLogger.info('Processing donation: DONOR-001');
                                        // => Info: normal operation

  zakatLogger.warning('Donation amount below threshold');
                                        // => Warning: potential issue

  // Error logging with context
  try {
    double amount = -100000.0;
    if (amount < 0) {
      throw ArgumentError('Amount cannot be negative: $amount');
    }
  } catch (e, stackTrace) {
    zakatLogger.error('Invalid donation amount', e, stackTrace);
                                        // => Error: with exception and stack
  }

  // Structured logging
  StructuredLogger structuredLogger = StructuredLogger('DonationAPI');

  structuredLogger.log(
    LogLevel.info,
    'Donation received',
    context: {
      'donorId': 'DONOR-001',
      'amount': 100000.0,
      'category': 'Zakat',
      'method': 'credit_card',
    },                                  // => Structured: queryable fields
  );

  structuredLogger.log(
    LogLevel.error,
    'Payment processing failed',
    context: {
      'donorId': 'DONOR-002',
      'amount': 200000.0,
      'errorCode': 'INSUFFICIENT_FUNDS',
      'retryCount': 3,
    },
  );

  // Performance logging
  Logger perfLogger = Logger('Performance');
  Stopwatch sw = Stopwatch()..start();

  // Simulate operation
  for (int i = 0; i < 1000000; i++) {
    double zakat = i * 0.025;
  }

  sw.stop();
  perfLogger.info('Zakat calculation completed in ${sw.elapsedMilliseconds}ms');

  // Request tracing
  String requestId = 'REQ-${DateTime.now().millisecondsSinceEpoch}';
  Logger traceLogger = Logger('API[$requestId]');
                                        // => Request-scoped: unique logger

  traceLogger.info('Request started');
  traceLogger.info('Validating input');
  traceLogger.info('Processing payment');
  traceLogger.info('Request completed');
}
```

**Logging Best Practices**:

| Practice               | Rationale                           | Example                             |
| ---------------------- | ----------------------------------- | ----------------------------------- |
| Structured logging     | Queryable logs in aggregation tools | JSON with fields                    |
| Log levels             | Filter by severity in production    | debug/info/warning/error/fatal      |
| Contextual information | Trace requests across services      | Request ID, user ID, transaction ID |
| Performance metrics    | Measure operation duration          | Stopwatch with elapsed time         |
| Error details          | Include exception and stack trace   | error(msg, exception, stackTrace)   |

**Key Takeaway**: Use structured logging with levels and context. Include timestamps, service name, and metadata. Log errors with exceptions and stack traces. Set minimum level for production. Add request IDs for tracing.

**Why It Matters**: Production issues are impossible to debug without logs. Structured logging enables log aggregation and querying (find all errors for user X). Log levels filter noise in production (info/warning/error only). Request IDs trace requests across microservices. Performance logs identify slow operations. In Flutter, logging tracks user journeys and crash context. For servers, logging is essential for incident response.

**Common Pitfalls**: Logging sensitive data (passwords, tokens). Too much debug logging in production (performance impact). Unstructured logs (hard to query). Missing context (can't trace request flow). No log rotation (disk fills up).

---

### Example 68: Error Handling and Monitoring

Production-grade error handling with monitoring integration for proactive issue detection.

```dart
// Custom error types
class DonationError implements Exception {
  final String message;
  final String code;
  final Map<String, dynamic>? context;

  DonationError(this.message, this.code, [this.context]);

  @override
  String toString() => 'DonationError[$code]: $message';
}

class ValidationError extends DonationError {
  ValidationError(String message, [Map<String, dynamic>? context])
      : super(message, 'VALIDATION_ERROR', context);
}

class PaymentError extends DonationError {
  PaymentError(String message, [Map<String, dynamic>? context])
      : super(message, 'PAYMENT_ERROR', context);
}

// Error monitor
class ErrorMonitor {
  static final List<Map<String, dynamic>> _errors = [];

  static void reportError(Object error, StackTrace stackTrace, {Map<String, dynamic>? context}) {
    Map<String, dynamic> errorReport = {
      'timestamp': DateTime.now().toIso8601String(),
      'error': error.toString(),
      'stackTrace': stackTrace.toString(),
      'context': context ?? {},
    };

    _errors.add(errorReport);

    // Production: send to monitoring service (Sentry, Bugsnag, Firebase Crashlytics)
    print('=== ERROR REPORTED ===');
    print(errorReport);
    print('======================\n');
  }

  static int get errorCount => _errors.length;

  static List<Map<String, dynamic>> getErrors({String? errorCode}) {
    if (errorCode == null) return _errors;

    return _errors.where((e) {
      String errorStr = e['error'] as String;
      return errorStr.contains(errorCode);
    }).toList();
  }
}

// Circuit breaker pattern
class CircuitBreaker {
  final int failureThreshold;           // => Max failures before opening
  final Duration timeout;               // => Time in open state before retry

  int _failureCount = 0;
  DateTime? _openedAt;
  bool _isOpen = false;

  CircuitBreaker({
    required this.failureThreshold,
    required this.timeout,
  });

  Future<T> execute<T>(Future<T> Function() operation) async {
    if (_isOpen) {
      // Check if timeout elapsed
      if (DateTime.now().difference(_openedAt!) > timeout) {
        _isOpen = false;                // => Half-open: try operation
        _failureCount = 0;
        print('Circuit breaker: HALF-OPEN (attempting retry)');
      } else {
        throw StateError('Circuit breaker is OPEN');
      }
    }

    try {
      T result = await operation();
      _failureCount = 0;                // => Success: reset counter
      return result;
    } catch (e, stackTrace) {
      _failureCount++;

      if (_failureCount >= failureThreshold) {
        _isOpen = true;
        _openedAt = DateTime.now();
        print('Circuit breaker: OPENED (failures: $_failureCount)');
      }

      ErrorMonitor.reportError(e, stackTrace);
      rethrow;
    }
  }

  bool get isOpen => _isOpen;
}

// Retry strategy
class RetryStrategy {
  final int maxAttempts;
  final Duration delay;

  RetryStrategy({required this.maxAttempts, required this.delay});

  Future<T> execute<T>(Future<T> Function() operation) async {
    int attempt = 0;

    while (true) {
      attempt++;

      try {
        return await operation();
      } catch (e, stackTrace) {
        if (attempt >= maxAttempts) {
          ErrorMonitor.reportError(e, stackTrace, context: {
            'maxAttempts': maxAttempts,
            'finalAttempt': attempt,
          });
          rethrow;
        }

        print('Retry $attempt/$maxAttempts after ${delay.inMilliseconds}ms');
        await Future.delayed(delay);
      }
    }
  }
}

void main() async {
  // Basic error handling
  try {
    throw ValidationError('Invalid donation amount', {
      'amount': -100000.0,
      'minAmount': 10000.0,
    });
  } on ValidationError catch (e, stackTrace) {
    ErrorMonitor.reportError(e, stackTrace);
  }

  // Circuit breaker
  CircuitBreaker breaker = CircuitBreaker(
    failureThreshold: 3,
    timeout: Duration(seconds: 5),
  );

  Future<double> unreliableOperation() async {
    // Simulate: flaky external service
    if (DateTime.now().millisecond % 2 == 0) {
      throw PaymentError('Service unavailable');
    }
    return 100000.0;
  }

  // Trigger circuit breaker
  for (int i = 0; i < 5; i++) {
    try {
      double amount = await breaker.execute(unreliableOperation);
      print('Success: Rp$amount\n');
    } catch (e) {
      print('Failed: $e\n');
    }
  }

  // Wait for circuit breaker timeout
  await Future.delayed(Duration(seconds: 6));
  print('After timeout - circuit breaker should retry:\n');

  try {
    double amount = await breaker.execute(() async => 100000.0);
    print('Success after timeout: Rp$amount\n');
  } catch (e) {
    print('Failed: $e\n');
  }

  // Retry strategy
  RetryStrategy retry = RetryStrategy(
    maxAttempts: 3,
    delay: Duration(milliseconds: 500),
  );

  int attemptCount = 0;
  try {
    await retry.execute(() async {
      attemptCount++;
      if (attemptCount < 3) {
        throw PaymentError('Temporary failure');
      }
      return 200000.0;
    });
    print('Retry succeeded on attempt $attemptCount\n');
  } catch (e) {
    print('Retry exhausted: $e\n');
  }

  // Error monitoring report
  print('=== Error Monitoring Report ===');
  print('Total errors: ${ErrorMonitor.errorCount}');
  print('Payment errors: ${ErrorMonitor.getErrors(errorCode: 'PAYMENT_ERROR').length}');
  print('Validation errors: ${ErrorMonitor.getErrors(errorCode: 'VALIDATION_ERROR').length}');
}
```

**Error Handling Patterns**:

| Pattern           | Use Case                  | Implementation               |
| ----------------- | ------------------------- | ---------------------------- |
| Custom exceptions | Domain-specific errors    | Extend Exception             |
| Error monitoring  | Track production issues   | Sentry, Firebase Crashlytics |
| Circuit breaker   | Prevent cascade failures  | Open after N failures        |
| Retry strategy    | Handle transient failures | Exponential backoff          |
| Fallback values   | Graceful degradation      | Return default on error      |

**Key Takeaway**: Use custom exception types for domain errors. Report errors to monitoring service with context. Implement circuit breaker to prevent cascade failures. Use retry strategies for transient errors.

**Why It Matters**: Production systems fail in unexpected ways. Custom exceptions provide clear error semantics. Error monitoring enables proactive issue detection before user reports. Circuit breakers prevent one failing service from cascading (saves entire system). Retry strategies handle transient network failures. In Flutter, error monitoring tracks crash rates and user impact. For servers, circuit breakers prevent downstream service overload.

**Common Pitfalls**: Catching generic Exception without specific handling. Not reporting errors to monitoring. Infinite retries without backoff. Circuit breaker without timeout (never recovers). Missing error context (can't reproduce issue).

---

### Example 69: Configuration Management

Managing application configuration across environments (development, staging, production).

```dart
enum Environment {
  development,
  staging,
  production,
}

class Config {
  final Environment environment;
  final String apiBaseUrl;
  final String databaseUrl;
  final int timeout;
  final bool enableLogging;
  final bool enableAnalytics;

  const Config({
    required this.environment,
    required this.apiBaseUrl,
    required this.databaseUrl,
    required this.timeout,
    required this.enableLogging,
    required this.enableAnalytics,
  });

  // Factory constructors for environments
  factory Config.development() {
    return const Config(
      environment: Environment.development,
      apiBaseUrl: 'http://localhost:8080',
      databaseUrl: 'mongodb://localhost:27017/dev',
      timeout: 5000,                    // => 5 seconds
      enableLogging: true,              // => Verbose logging
      enableAnalytics: false,           // => No tracking in dev
    );
  }

  factory Config.staging() {
    return const Config(
      environment: Environment.staging,
      apiBaseUrl: 'https://staging-api.example.com',
      databaseUrl: 'mongodb://staging-db:27017/staging',
      timeout: 10000,                   // => 10 seconds
      enableLogging: true,
      enableAnalytics: true,            // => Test analytics
    );
  }

  factory Config.production() {
    return const Config(
      environment: Environment.production,
      apiBaseUrl: 'https://api.example.com',
      databaseUrl: 'mongodb://prod-db:27017/production',
      timeout: 30000,                   // => 30 seconds
      enableLogging: false,             // => Minimal logging
      enableAnalytics: true,
    );
  }

  // Load from environment variable
  factory Config.fromEnvironment() {
    const String env = String.fromEnvironment('ENV', defaultValue: 'development');
                                        // => Read: compile-time constant
                                        // => Build: dart compile -DENV=production

    switch (env) {
      case 'staging':
        return Config.staging();
      case 'production':
        return Config.production();
      default:
        return Config.development();
    }
  }

  bool get isDevelopment => environment == Environment.development;
  bool get isProduction => environment == Environment.production;
}

// Configuration provider (singleton)
class AppConfig {
  static AppConfig? _instance;
  final Config config;

  AppConfig._(this.config);

  factory AppConfig.initialize(Config config) {
    _instance = AppConfig._(config);
    return _instance!;
  }

  static AppConfig get instance {
    if (_instance == null) {
      throw StateError('AppConfig not initialized');
    }
    return _instance!;
  }

  static Config get current => instance.config;
}

// Feature flags
class FeatureFlags {
  final Map<String, bool> _flags = {};

  void enable(String feature) {
    _flags[feature] = true;
  }

  void disable(String feature) {
    _flags[feature] = false;
  }

  bool isEnabled(String feature) {
    return _flags[feature] ?? false;    // => Default: disabled
  }
}

void main() {
  // Initialize configuration
  AppConfig.initialize(Config.development());
                                        // => One-time initialization

  Config config = AppConfig.current;

  print('=== Configuration ===');
  print('Environment: ${config.environment}');
  print('API URL: ${config.apiBaseUrl}');
  print('Database: ${config.databaseUrl}');
  print('Timeout: ${config.timeout}ms');
  print('Logging: ${config.enableLogging}');
  print('Analytics: ${config.enableAnalytics}');
  print('');

  // Environment-specific behavior
  if (config.isDevelopment) {
    print('Development mode: verbose logging enabled');
  } else if (config.isProduction) {
    print('Production mode: optimizations enabled');
  }

  // Feature flags
  FeatureFlags features = FeatureFlags();
  features.enable('new_donation_flow');
  features.enable('zakat_calculator_v2');
  features.disable('legacy_payment');

  if (features.isEnabled('new_donation_flow')) {
    print('\nUsing new donation flow');
  }

  if (features.isEnabled('zakat_calculator_v2')) {
    print('Using Zakat calculator v2');
  }

  if (!features.isEnabled('legacy_payment')) {
    print('Legacy payment disabled');
  }

  // Conditional compilation
  if (config.isDevelopment) {
    print('\n=== Debug Info ===');
    print('This only appears in development');
  }

  // Different configs
  print('\n=== Environment Comparison ===');
  print('Development API: ${Config.development().apiBaseUrl}');
  print('Staging API: ${Config.staging().apiBaseUrl}');
  print('Production API: ${Config.production().apiBaseUrl}');
}
```

**Configuration Best Practices**:

| Practice               | Rationale                          | Example                        |
| ---------------------- | ---------------------------------- | ------------------------------ |
| Environment separation | Different settings per environment | dev/staging/production configs |
| Singleton pattern      | Single source of truth             | AppConfig.current              |
| Const constructors     | Compile-time constants             | const Config(...)              |
| Feature flags          | Toggle features without redeploy   | FeatureFlags                   |
| No secrets in code     | Security risk                      | Load from env vars or vault    |

**Key Takeaway**: Use factory constructors for environment-specific configs. Initialize once at startup. Access via singleton. Use feature flags for gradual rollouts. Never hardcode secrets.

**Why It Matters**: Hardcoded configuration couples code to environment, making deployment error-prone. Environment-specific configs enable same codebase for dev/staging/prod. Feature flags allow testing in production with limited users (canary deployments). Configuration as code enables version control and code review. In Flutter, configs manage API endpoints, feature flags, and analytics. For servers, configs manage database connections, timeouts, and third-party integrations.

**Common Pitfalls**: Hardcoding production URLs in code. Committing secrets to version control. Not validating configuration at startup. Mutable configuration state (use const/final). Missing environment-specific overrides.

---

### Example 70: Dependency Injection Pattern

Implementing dependency injection for testable, maintainable code by inverting dependencies.

```dart
// Interfaces (abstract classes)
abstract class DonationRepository {
  Future<void> saveDonation(Donation donation);
  Future<Donation?> getDonation(String id);
  Future<List<Donation>> getAllDonations();
}

abstract class PaymentGateway {
  Future<bool> processPayment(double amount, String method);
}

abstract class Logger {
  void info(String message);
  void error(String message, [Object? error]);
}

// Models
class Donation {
  final String id;
  final String donor;
  final double amount;

  Donation(this.id, this.donor, this.amount);
}

// Concrete implementations
class InMemoryDonationRepository implements DonationRepository {
  final Map<String, Donation> _storage = {};

  @override
  Future<void> saveDonation(Donation donation) async {
    _storage[donation.id] = donation;
  }

  @override
  Future<Donation?> getDonation(String id) async {
    return _storage[id];
  }

  @override
  Future<List<Donation>> getAllDonations() async {
    return _storage.values.toList();
  }
}

class MockPaymentGateway implements PaymentGateway {
  @override
  Future<bool> processPayment(double amount, String method) async {
    await Future.delayed(Duration(milliseconds: 100));
    return amount > 0;                  // => Mock: always succeeds for positive amounts
  }
}

class ConsoleLogger implements Logger {
  @override
  void info(String message) {
    print('[INFO] $message');
  }

  @override
  void error(String message, [Object? error]) {
    print('[ERROR] $message');
    if (error != null) {
      print('  Error: $error');
    }
  }
}

// Service with dependency injection
class DonationService {
  final DonationRepository repository;  // => Dependency: injected
  final PaymentGateway gateway;         // => Dependency: injected
  final Logger logger;                  // => Dependency: injected

  // Constructor injection
  DonationService({
    required this.repository,
    required this.gateway,
    required this.logger,
  });                                   // => Dependencies: provided by caller

  Future<bool> createDonation(String donor, double amount, String method) async {
    logger.info('Creating donation: $donor - Rp$amount');

    try {
      // Process payment
      bool paymentSuccess = await gateway.processPayment(amount, method);
      if (!paymentSuccess) {
        logger.error('Payment failed for $donor');
        return false;
      }

      // Save donation
      Donation donation = Donation(
        'DON-${DateTime.now().millisecondsSinceEpoch}',
        donor,
        amount,
      );
      await repository.saveDonation(donation);

      logger.info('Donation saved: ${donation.id}');
      return true;
    } catch (e) {
      logger.error('Error creating donation', e);
      return false;
    }
  }

  Future<List<Donation>> getAllDonations() async {
    logger.info('Fetching all donations');
    return await repository.getAllDonations();
  }
}

// Service locator (simple DI container)
class ServiceLocator {
  static final Map<Type, dynamic> _services = {};

  static void register<T>(T service) {
    _services[T] = service;             // => Register: service by type
  }

  static T get<T>() {
    if (!_services.containsKey(T)) {
      throw StateError('Service $T not registered');
    }
    return _services[T] as T;
  }

  static void clear() {
    _services.clear();
  }
}

void main() async {
  // Manual dependency injection
  DonationRepository repository = InMemoryDonationRepository();
  PaymentGateway gateway = MockPaymentGateway();
  Logger logger = ConsoleLogger();

  DonationService service = DonationService(
    repository: repository,             // => Inject: repository
    gateway: gateway,                   // => Inject: gateway
    logger: logger,                     // => Inject: logger
  );

  // Use service
  await service.createDonation('Ahmad', 100000.0, 'credit_card');
  await service.createDonation('Fatimah', 200000.0, 'bank_transfer');

  List<Donation> donations = await service.getAllDonations();
  print('\nTotal donations: ${donations.length}');

  // Service locator pattern
  print('\n=== Service Locator ===');
  ServiceLocator.register<DonationRepository>(InMemoryDonationRepository());
  ServiceLocator.register<PaymentGateway>(MockPaymentGateway());
  ServiceLocator.register<Logger>(ConsoleLogger());

  DonationService service2 = DonationService(
    repository: ServiceLocator.get<DonationRepository>(),
    gateway: ServiceLocator.get<PaymentGateway>(),
    logger: ServiceLocator.get<Logger>(),
  );

  await service2.createDonation('Ali', 150000.0, 'ewallet');

  // Benefits of DI:
  print('\n=== DI Benefits ===');
  print('✅ Testable: swap implementations (mock for testing)');
  print('✅ Flexible: change implementations without modifying service');
  print('✅ Maintainable: dependencies explicit in constructor');
  print('✅ Decoupled: service doesn\'t know concrete implementations');
}
```

**Dependency Injection Patterns**:

| Pattern               | Implementation                   | Use Case                                |
| --------------------- | -------------------------------- | --------------------------------------- |
| Constructor injection | Pass dependencies to constructor | Most common, clear dependencies         |
| Setter injection      | Set dependencies via setters     | Optional dependencies                   |
| Service locator       | Global registry of services      | Simple DI container                     |
| DI frameworks         | get_it, provider, riverpod       | Flutter apps, complex dependency graphs |

**Key Takeaway**: Use constructor injection to provide dependencies. Depend on abstractions (interfaces), not concrete implementations. Enables testing with mocks. Service locator for simple DI container.

**Why It Matters**: Hardcoded dependencies make testing impossible (can't swap real database for test mock). DI inverts dependencies, allowing flexible implementations (in-memory for tests, PostgreSQL for production). Constructor injection makes dependencies explicit and visible. In Flutter, DI frameworks (Provider, Riverpod) manage widget dependencies and state. For servers, DI enables swapping implementations across environments.

**Common Pitfalls**: Service locator is anti-pattern in large apps (hidden dependencies). Not using interfaces defeats DI purpose. Circular dependencies (A depends on B, B depends on A). Too many constructor parameters (split into smaller services).

---

### Example 71: Repository Pattern

Implementing repository pattern for data access abstraction, separating business logic from data layer.

```dart
// Domain model
// => Core business entity representing a donation
class Donation {
  final String id;                       // => Unique donation identifier
  final String donor;                    // => Donor name (simplified - real app uses donor ID)
  final double amount;                   // => Donation amount in IDR
  final DateTime timestamp;              // => When donation occurred

  // => Named constructor for creating donation instances
  Donation({
    required this.id,                    // => All fields required (non-nullable)
    required this.donor,
    required this.amount,
    required this.timestamp,
  });
  // => All fields final (immutable value object)

  // => Serialization method: converts object to JSON map
  Map<String, dynamic> toJson() => {
    'id': id,                            // => Preserve ID for database storage
    'donor': donor,                      // => Preserve donor name
    'amount': amount,                    // => Amount stored as number
    'timestamp': timestamp.toIso8601String(),
    // => Convert DateTime to ISO 8601 string for JSON compatibility
  };
  // => Used when saving to file or database

  // => Deserialization factory: creates object from JSON map
  factory Donation.fromJson(Map<String, dynamic> json) => Donation(
    id: json['id'],                      // => Extract ID from JSON
    donor: json['donor'],                // => Extract donor from JSON
    amount: json['amount'],              // => Extract amount (already double)
    timestamp: DateTime.parse(json['timestamp']),
    // => Parse ISO 8601 string back to DateTime
  );
  // => Used when loading from file or database
  // => Enables round-trip serialization (toJson → fromJson)

// Repository interface
// => Repository pattern: abstract data access behind interface
// => Defines contract for donation data operations (CRUD + queries)
abstract class DonationRepository {
  // => Create operation: persists new donation
  Future<void> create(Donation donation);
  // => Async operation (database I/O)
  // => Void return (no result, just side effect)

  // => Read operation: retrieves donation by ID
  Future<Donation?> findById(String id);
  // => Returns nullable (donation may not exist)
  // => Primary key lookup (single result)

  // => Read operation: retrieves all donations
  Future<List<Donation>> findAll();
  // => Returns list (may be empty if no donations)
  // => Use with caution in production (unbounded result set)

  // => Query operation: finds donations by donor name
  Future<List<Donation>> findByDonor(String donor);
  // => Returns list of donations for specific donor
  // => Enables reporting, donor history views

  // => Update operation: modifies existing donation
  Future<void> update(Donation donation);
  // => Replaces donation with same ID
  // => Void return (no result, just side effect)

  // => Delete operation: removes donation
  Future<void> delete(String id);
  // => Removes by ID
  // => Void return (no error if ID doesn't exist - idempotent)
}
// => Interface allows multiple implementations (in-memory, file, database)
// => Business logic depends on interface, not concrete implementation

// In-memory implementation (testing)
// => Test double: simulates database using in-memory Map
// => Fast (no I/O), isolated (no shared state), deterministic (no race conditions)
class InMemoryDonationRepository implements DonationRepository {
  // => Storage: Map simulating database table
  final Map<String, Donation> _storage = {};
  // => Key: donation ID (primary key), Value: donation object
  // => Private field (encapsulation - only accessible via interface methods)

  @override
  // => Implements create operation from interface
  Future<void> create(Donation donation) async {
    // => Marked async to match interface (even though no actual async work)
    _storage[donation.id] = donation;
    // => Store donation in map (upsert behavior)
    print('[MEMORY] Created: ${donation.id}');
    // => Log for visibility during example execution
  }

  @override
  // => Implements findById query from interface
  Future<Donation?> findById(String id) async {
    return _storage[id];
    // => Map lookup by key
    // => Returns null if ID doesn't exist (matches interface contract)
  }

  @override
  // => Implements findAll query from interface
  Future<List<Donation>> findAll() async {
    return _storage.values.toList();
    // => Convert map values to list
    // => Returns all donations (no filtering)
    // => Empty list if no donations exist
  }

  @override
  // => Implements findByDonor query from interface
  Future<List<Donation>> findByDonor(String donor) async {
    return _storage.values
        // => Start with all donations
        .where((d) => d.donor == donor)
        // => Filter to only donations where donor name matches
        .toList();
        // => Convert filtered result to list
    // => Returns empty list if donor has no donations
  }

  @override
  // => Implements update operation from interface
  Future<void> update(Donation donation) async {
    _storage[donation.id] = donation;
    // => Replace existing donation with same ID
    // => Uses same logic as create (map upsert)
    print('[MEMORY] Updated: ${donation.id}');
    // => Log for visibility
  }

  @override
  // => Implements delete operation from interface
  Future<void> delete(String id) async {
    _storage.remove(id);
    // => Remove donation from map
    // => No-op if ID doesn't exist (idempotent behavior)
    print('[MEMORY] Deleted: $id');
    // => Log for visibility
  }
}
// => End of in-memory implementation
// => Used for tests (fast, isolated) and development (no database setup)

// File-based implementation (persistence)
// => Production-like implementation: persists data to file
// => Same interface as in-memory version (demonstrates pattern benefit)
class FileDonationRepository implements DonationRepository {
  final String filePath;                 // => Path to JSON file for storage

  // => Constructor requires file path
  FileDonationRepository(this.filePath);

  // => Helper method: loads all donations from file
  Future<Map<String, Donation>> _load() async {
    // Simulated file I/O
    // => Real implementation would:
    // => 1. Read file contents as string
    // => 2. Parse JSON string to Map
    // => 3. Convert JSON objects to Donation instances via fromJson
    print('[FILE] Loading from $filePath');
    return {};                          // => Would read from actual file
    // => Simulated: returns empty map (no file operations in example)
  }

  // => Helper method: saves all donations to file
  Future<void> _save(Map<String, Donation> data) async {
    // Simulated file I/O
    // => Real implementation would:
    // => 1. Convert Donation instances to JSON via toJson
    // => 2. Convert Map to JSON string
    // => 3. Write string to file
    print('[FILE] Saving to $filePath');
                                        // => Would write to actual file
    // => Simulated: just logs (no file operations in example)
  }

  @override
  // => Implements create operation from interface
  Future<void> create(Donation donation) async {
    Map<String, Donation> data = await _load();
    // => Load existing donations from file
    data[donation.id] = donation;
    // => Add new donation to in-memory map
    await _save(data);
    // => Save updated map back to file
    // => Pattern: read-modify-write (not optimized for concurrent writes)
  }

  @override
  // => Implements findById query from interface
  Future<Donation?> findById(String id) async {
    Map<String, Donation> data = await _load();
    // => Load all donations from file
    return data[id];
    // => Lookup donation by ID
    // => Returns null if not found
  }

  @override
  // => Implements findAll query from interface
  Future<List<Donation>> findAll() async {
    Map<String, Donation> data = await _load();
    // => Load all donations from file
    return data.values.toList();
    // => Convert map values to list
    // => Returns all donations
  }

  @override
  // => Implements findByDonor query from interface
  Future<List<Donation>> findByDonor(String donor) async {
    Map<String, Donation> data = await _load();
    // => Load all donations from file
    return data.values
        // => Iterate through all donations
        .where((d) => d.donor == donor)
        // => Filter to matching donor name
        .toList();
        // => Convert filtered result to list
  }

  @override
  // => Implements update operation from interface
  Future<void> update(Donation donation) async {
    Map<String, Donation> data = await _load();
    // => Load existing donations
    data[donation.id] = donation;
    // => Replace donation with same ID (upsert)
    await _save(data);
    // => Save updated data to file
  }

  @override
  // => Implements delete operation from interface
  Future<void> delete(String id) async {
    Map<String, Donation> data = await _load();
    // => Load existing donations
    data.remove(id);
    // => Remove donation from map
    await _save(data);
    // => Save updated data to file (without deleted donation)
  }
}
// => End of file implementation
// => Demonstrates repository pattern: same interface, different storage mechanism
// => Business logic doesn't know if using in-memory or file repository

// Service using repository
// => Business logic layer: depends on repository abstraction
// => Demonstrates dependency injection and loose coupling
class DonationService {
  final DonationRepository repository;  // => Abstraction: not concrete type
  // => Type is interface (DonationRepository), not implementation
  // => Enables swapping implementations without changing service code

  // => Constructor injection: dependency provided by caller
  DonationService(this.repository);
  // => Service doesn't create repository (inversion of control)
  // => Makes dependencies explicit and enables testing

  // => Business method: creates donation with generated ID
  Future<void> createDonation(String donor, double amount) async {
    // => Build donation object with auto-generated ID
    Donation donation = Donation(
      id: 'DON-${DateTime.now().millisecondsSinceEpoch}',
      // => Simple ID generation using timestamp
      // => Production: use UUID or database-generated ID
      donor: donor,                      // => Donor name from caller
      amount: amount,                    // => Amount from caller
      timestamp: DateTime.now(),         // => Current timestamp
    );

    // => Delegate to repository for persistence
    await repository.create(donation);
    // => Service doesn't know if this goes to memory, file, or database
    // => Repository abstraction provides flexibility
    print('Service: Donation created');
    // => Log for visibility
  }

  // => Business method: calculates total donations for donor
  Future<double> getTotalByDonor(String donor) async {
    // => Query repository for donor's donations
    List<Donation> donations = await repository.findByDonor(donor);
    // => Repository returns list of all donations for this donor
    return donations.fold(0.0, (sum, d) => sum + d.amount);
    // => fold: reduce list to single value
    // => Starting value: 0.0 (empty sum)
    // => Accumulator: sum + d.amount (add each donation amount)
    // => Returns total of all donations for donor
  }
}
// => End of service class
// => Service uses repository abstraction (not tied to specific implementation)

// => Main function demonstrates repository pattern benefits
void main() async {
  // => Marked async because it calls async service methods

  // Use in-memory repository (development/testing)
  // => Scenario 1: Development/testing with in-memory storage
  print('=== In-Memory Repository ===');
  DonationRepository memoryRepo = InMemoryDonationRepository();
  // => Create in-memory implementation
  // => Type: DonationRepository (interface), not InMemoryDonationRepository
  DonationService service1 = DonationService(memoryRepo);
  // => Inject in-memory repository into service
  // => Service doesn't know it's using in-memory implementation

  // => Create test donations
  await service1.createDonation('Ahmad', 100000.0);
  // => Creates donation: Ahmad, 100k IDR
  // => Stored in memory map
  await service1.createDonation('Ahmad', 150000.0);
  // => Creates second donation: Ahmad, 150k IDR
  // => Same donor, different donation
  await service1.createDonation('Fatimah', 200000.0);
  // => Creates donation: Fatimah, 200k IDR
  // => Different donor

  // => Query total for specific donor
  double ahmadTotal = await service1.getTotalByDonor('Ahmad');
  // => Calls service method which queries repository
  // => Should return 250000.0 (100k + 150k)
  print('Ahmad total: Rp$ahmadTotal\n');
  // => Output: Rp250000.0

  // Switch to file repository (production)
  // => Scenario 2: Production with file-based persistence
  print('=== File Repository ===');
  DonationRepository fileRepo = FileDonationRepository('donations.json');
  // => Create file implementation (would persist to donations.json)
  // => Type: DonationRepository (same interface as in-memory version)
  DonationService service2 = DonationService(fileRepo);
  // => Inject file repository into service
  // => Service code unchanged (depends on abstraction)

  // => Create donation using file repository
  await service2.createDonation('Ali', 175000.0);
  // => Creates donation: Ali, 175k IDR
  // => Would be saved to file (simulated in example)
  // => Same service method, different storage mechanism

  // => Summary of repository pattern benefits
  print('\n=== Repository Pattern Benefits ===');
  print('✅ Abstraction: business logic doesn\'t know storage mechanism');
  // => Service1 and service2 use same code, different storage
  print('✅ Testability: swap file repo for in-memory in tests');
  // => Tests use in-memory (fast, isolated), production uses file/database
  print('✅ Flexibility: change storage (file → database) without changing service');
  // => Can add PostgreSQL implementation without touching DonationService
  print('✅ Single Responsibility: repository handles data access only');
  // => Service handles business logic, repository handles persistence
}
// => Output demonstrates same service working with different repositories
// => Pattern enables swapping storage implementations without code changes
```

**Repository Pattern Benefits**:

| Benefit        | Description                              | Example                           |
| -------------- | ---------------------------------------- | --------------------------------- |
| Abstraction    | Business logic independent of data layer | Service uses interface            |
| Testability    | Easy to mock for unit tests              | InMemoryRepository for tests      |
| Centralization | Single place for data access logic       | All queries in repository         |
| Flexibility    | Swap implementations (file, DB, API)     | File → PostgreSQL without changes |

**Key Takeaway**: Use repository pattern to abstract data access. Define interface with CRUD operations. Implement multiple versions (in-memory, file, database). Business logic depends on abstraction, not concrete repository.

**Why It Matters**: Hardcoded data access couples business logic to storage mechanism. Repository pattern decouples storage from logic, enabling database changes without touching business code. In-memory repository enables fast tests without database setup. In Flutter, repository pattern separates API calls from UI logic. For servers, repository enables polyglot persistence (different databases for different data).

**Common Pitfalls**: Repository exposing storage implementation details (SQL queries in interface). Too many repository methods (keep focused on entity). Missing abstraction (depend on concrete repository). Not using repository in tests (defeats testability purpose).

---

### Example 72: Service Layer Pattern

Implementing service layer to orchestrate business logic across multiple repositories and external services.

```dart
// Repositories
// => Data access layer abstractions (repository pattern)
// => Service layer depends on interfaces, not implementations
abstract class DonationRepository {
  // => Persists donation to data store
  Future<void> save(Donation donation);
  // => Returns Future<void> - async operation, no return value

  // => Retrieves donation by unique identifier
  Future<Donation?> findById(String id);
  // => Returns nullable Donation (may not exist)
  // => Enables service to check if donation already processed
}

// => Repository for donor data access
abstract class DonorRepository {
  // => Finds donor by email address (business key)
  Future<Donor?> findByEmail(String email);
  // => Returns nullable Donor (new donors don't exist yet)
  // => Service uses this to get-or-create donor pattern

  // => Saves or updates donor record
  Future<void> save(Donor donor);
  // => Used for both creating new donors and updating stats
}

// External services
// => Third-party service abstractions (adapter pattern)
// => Allows mocking for tests, swapping implementations
abstract class PaymentGateway {
  // => Processes payment through external provider
  Future<PaymentResult> processPayment(double amount, String method);
  // => amount: payment amount in IDR
  // => method: payment method identifier ('credit_card', 'bank_transfer', etc.)
  // => Returns result indicating success/failure with details
}

// => Email notification service abstraction
abstract class EmailService {
  // => Sends donation receipt via email
  Future<void> sendReceipt(String email, Donation donation);
  // => email: recipient email address
  // => donation: donation details to include in receipt
  // => Fire-and-forget operation (doesn't block on email delivery)
}

// Models
// => Domain models representing business entities
class Donation {
  final String id;                       // => Unique donation identifier
  final String donorEmail;               // => Email of donor (foreign key)
  final double amount;                   // => Donation amount in IDR
  final DateTime timestamp;              // => When donation occurred

  // => Constructor for creating donation instances
  Donation(this.id, this.donorEmail, this.amount, this.timestamp);
  // => All fields immutable (final) - value object pattern
}

// => Donor entity representing person who donates
class Donor {
  final String email;                    // => Unique identifier (business key)
  final String name;                     // => Donor's display name
  int totalDonations;                    // => Count of donations made
  double lifetimeAmount;                 // => Total donated over time

  // => Constructor for creating donor instances
  Donor(this.email, this.name, this.totalDonations, this.lifetimeAmount);
  // => totalDonations and lifetimeAmount are mutable (updated on new donations)
  // => email and name are immutable (business constraint)
}

// => Result object from payment gateway operations
class PaymentResult {
  final bool success;                    // => Whether payment succeeded
  final String? transactionId;           // => Payment provider transaction ID (if successful)
  final String? errorMessage;            // => Error description (if failed)

  // => Named constructor for flexible initialization
  PaymentResult({required this.success, this.transactionId, this.errorMessage});
  // => success required, transaction ID and error optional
  // => Successful payment has transactionId, failed payment has errorMessage
}

// Service layer (orchestrates business logic)
// => Service coordinates multiple repositories and external services
// => Implements complex business workflows across system boundaries
class DonationService {
  // => Dependencies injected via constructor (dependency injection pattern)
  final DonationRepository donationRepo;  // => Data access for donations
  final DonorRepository donorRepo;        // => Data access for donors
  final PaymentGateway paymentGateway;    // => External payment processing
  final EmailService emailService;        // => External email notifications

  // => Constructor requires all dependencies (makes dependencies explicit)
  DonationService({
    required this.donationRepo,
    required this.donorRepo,
    required this.paymentGateway,
    required this.emailService,
  });
  // => Named parameters enforce clarity at call site
  // => All dependencies required (no nullable dependencies)
  // => Enables testing with mocks (dependency injection)

  // Complex operation orchestrating multiple components
  // => Main business workflow: process a donation from start to finish
  // => Coordinates 4 external dependencies in specific sequence
  Future<DonationResult> processDonation({
    required String donorEmail,          // => Donor's email address
    required String donorName,           // => Donor's name (for new donors)
    required double amount,              // => Donation amount in IDR
    required String paymentMethod,       // => Payment method identifier
  }) async {
    // => async method because it orchestrates multiple async operations
    print('\n=== Processing Donation ===');
    // => Logging provides visibility into multi-step workflow

    // Step 1: Get or create donor
    // => Business rule: must have donor record before processing donation
    print('1. Getting donor...');
    Donor? donor = await donorRepo.findByEmail(donorEmail);
    // => Query repository by email (business key)
    // => Returns null if donor doesn't exist yet (first-time donor)
    if (donor == null) {
      // => Donor not found - create new donor record
      print('   Creating new donor');
      donor = Donor(donorEmail, donorName, 0, 0.0);
      // => Initialize with zero donations and zero lifetime amount
      await donorRepo.save(donor);
      // => Persist new donor to database
      // => After this, donor variable is non-null (fresh donor record)
    }
    // => After this step: donor variable guaranteed non-null

    // Step 2: Process payment
    // => Critical step: must charge donor before recording donation
    print('2. Processing payment...');
    PaymentResult paymentResult = await paymentGateway.processPayment(
      amount,
      paymentMethod,
    );
    // => Call external payment provider (Stripe, PayPal, etc.)
    // => This is async - may take seconds for network round-trip
    // => Payment provider returns success/failure result

    // => Handle payment failure (business rule: no payment = no donation)
    if (!paymentResult.success) {
      // => Payment failed - abort workflow and return error
      print('   Payment failed: ${paymentResult.errorMessage}');
      return DonationResult(
        success: false,
        errorMessage: 'Payment failed: ${paymentResult.errorMessage}',
      );
      // => Early return prevents subsequent steps from executing
      // => No donation record created, no donor stats updated
      // => System state remains consistent (no partial success)
    }

    // => Payment succeeded - continue workflow
    print('   Payment successful: ${paymentResult.transactionId}');
    // => transactionId is payment provider reference for reconciliation

    // Step 3: Save donation
    // => Payment succeeded - record donation for auditing and reporting
    print('3. Saving donation...');
    Donation donation = Donation(
      'DON-${DateTime.now().millisecondsSinceEpoch}',
      // => Generate unique ID using timestamp (simple strategy)
      // => Production: use UUID or database-generated ID
      donorEmail,                        // => Link donation to donor
      amount,                            // => Record amount donated
      DateTime.now(),                    // => Timestamp for when donation occurred
    );
    // => Create donation object with all required fields
    await donationRepo.save(donation);
    // => Persist to database (creates permanent record)
    // => After this: donation exists in database with unique ID

    // Step 4: Update donor statistics
    // => Business rule: maintain aggregate stats on donor record
    print('4. Updating donor statistics...');
    donor.totalDonations++;              // => Increment donation counter
    // => donor variable still valid from step 1
    donor.lifetimeAmount += amount;      // => Add to lifetime total
    // => These stats enable donor recognition (top donors, badges, etc.)
    await donorRepo.save(donor);
    // => Persist updated stats to database
    // => Same save() method used for create and update

    // Step 5: Send receipt email
    // => Business requirement: email confirmation to donor
    print('5. Sending receipt email...');
    await emailService.sendReceipt(donorEmail, donation);
    // => Sends formatted email with donation details
    // => This is fire-and-forget (workflow doesn't wait for email delivery)
    // => Email failure doesn't rollback donation (already committed)

    print('=== Donation Complete ===\n');
    // => All 5 steps completed successfully

    // => Return success result with donation and transaction references
    return DonationResult(
      success: true,                     // => Workflow succeeded
      donationId: donation.id,           // => Reference for donation record
      transactionId: paymentResult.transactionId,
      // => Reference for payment provider transaction
      // => Enables reconciliation between internal records and payment provider
    );
    // => Caller receives confirmation with references to both systems
  }
  // => End of complex multi-step workflow
  // => Service coordinated 4 dependencies across 5 steps
  // => Maintains data consistency (all-or-nothing on payment failure)

  // => Query method: retrieves donor statistics for reporting
  Future<DonorStats> getDonorStatistics(String email) async {
    // => Looks up donor by email (business key)
    Donor? donor = await donorRepo.findByEmail(email);
    // => Repository query returns nullable result
    if (donor == null) {
      // => Donor doesn't exist - return zero stats (not an error)
      return DonorStats(totalDonations: 0, lifetimeAmount: 0.0);
      // => Enables "donor has never donated" UI state
    }

    // => Donor exists - return their stats
    return DonorStats(
      totalDonations: donor.totalDonations,
      // => Count of donations made by donor
      lifetimeAmount: donor.lifetimeAmount,
      // => Total amount donated over time
    );
    // => Maps from Donor entity to DonorStats DTO
    // => Separates internal model from API response
  }
}
// => End of DonationService class
// => Service encapsulates all donation-related business logic

// => Result object returned from processDonation
class DonationResult {
  final bool success;                    // => Overall operation success/failure
  final String? donationId;              // => Internal donation ID (if successful)
  final String? transactionId;           // => Payment provider transaction ID (if successful)
  final String? errorMessage;            // => Error description (if failed)

  // => Named constructor for flexible result creation
  DonationResult({
    required this.success,               // => success is always required
    this.donationId,                     // => Optional: only present on success
    this.transactionId,                  // => Optional: only present on success
    this.errorMessage,                   // => Optional: only present on failure
  });
  // => Pattern: success=true has IDs, success=false has errorMessage
  // => Caller checks success field to determine workflow outcome
}

// => DTO for donor statistics query
class DonorStats {
  final int totalDonations;              // => Count of donations
  final double lifetimeAmount;           // => Total donated in IDR

  // => Named constructor requires both fields
  DonorStats({required this.totalDonations, required this.lifetimeAmount});
  // => Immutable value object (all fields final)
  // => Used for reporting, UI display, donor recognition
}

// Mock implementations
// => Test doubles for running example without real database/external services
// => Production code would use real implementations (PostgreSQL, Stripe, SendGrid)
class MockDonationRepository implements DonationRepository {
  // => In-memory storage simulating database
  final Map<String, Donation> _storage = {};
  // => Key: donation ID, Value: donation object
  // => Resets on each program run (not persistent)

  @override
  // => Implements save operation from interface
  Future<void> save(Donation donation) async {
    // => Simulate async database operation
    _storage[donation.id] = donation;
    // => Store donation in map (upsert behavior)
    // => Same ID overwrites previous donation (update)
    // => New ID creates new entry (insert)
  }

  @override
  // => Implements findById query from interface
  Future<Donation?> findById(String id) async {
    // => Simulate async database query
    return _storage[id];
    // => Map lookup returns null if key doesn't exist
    // => Matches repository interface contract (nullable return)
  }
}

// => Mock implementation of donor repository
class MockDonorRepository implements DonorRepository {
  // => In-memory storage simulating donor database table
  final Map<String, Donor> _storage = {};
  // => Key: email address, Value: donor object
  // => Email used as business key (unique identifier)

  @override
  // => Implements findByEmail query from interface
  Future<Donor?> findByEmail(String email) async {
    // => Simulate async database query
    return _storage[email];
    // => Lookup by email (business key)
    // => Returns null if donor not found (first-time donor)
  }

  @override
  // => Implements save operation from interface
  Future<void> save(Donor donor) async {
    // => Simulate async database operation
    _storage[donor.email] = donor;
    // => Store donor in map keyed by email
    // => Upsert: creates if new, updates if exists
  }
}

// => Mock implementation of payment gateway
class MockPaymentGateway implements PaymentGateway {
  @override
  // => Simulates external payment provider API call
  Future<PaymentResult> processPayment(double amount, String method) async {
    // => Simulate network latency (realistic delay)
    await Future.delayed(Duration(milliseconds: 100));
    // => Real payment would make HTTP request to Stripe/PayPal/etc.
    // => Mock always succeeds (simplification for example)
    return PaymentResult(
      success: true,                     // => Mock always returns success
      transactionId: 'TXN-${DateTime.now().millisecondsSinceEpoch}',
      // => Generate fake transaction ID using timestamp
      // => Real payment provider would return their transaction ID
    );
    // => Production mock could simulate failures for testing error paths
  }
}

// => Mock implementation of email service
class MockEmailService implements EmailService {
  @override
  // => Simulates sending email via provider (SendGrid, AWS SES, etc.)
  Future<void> sendReceipt(String email, Donation donation) async {
    // => Simulate network latency
    await Future.delayed(Duration(milliseconds: 50));
    // => Real email would make HTTP request to email provider
    print('   Email sent to $email');
    // => Mock just logs to console (doesn't send real email)
    // => Production mock could capture emails for testing
  }
}

// => Main function demonstrates service layer pattern in action
void main() async {
  // => Marked async because it calls async service methods

  // Setup service with dependencies
  // => Dependency injection: manually wire up dependencies
  DonationService service = DonationService(
    donationRepo: MockDonationRepository(),
    // => Pass mock donation repository (in-memory storage)
    donorRepo: MockDonorRepository(),
    // => Pass mock donor repository (in-memory storage)
    paymentGateway: MockPaymentGateway(),
    // => Pass mock payment gateway (simulated payments)
    emailService: MockEmailService(),
    // => Pass mock email service (console logging)
  );
  // => Service doesn't know these are mocks (depends on interfaces)
  // => Production: inject real implementations (PostgreSQL, Stripe, SendGrid)

  // Process donations
  // => Scenario: First-time donor makes donation
  DonationResult result1 = await service.processDonation(
    donorEmail: 'ahmad@example.com',
    donorName: 'Ahmad',               // => New donor name
    amount: 100000.0,                 // => 100k IDR donation
    paymentMethod: 'credit_card',     // => Payment method
  );
  // => This triggers full 5-step workflow:
  // => 1. Creates new donor (first time)
  // => 2. Processes payment (100k)
  // => 3. Saves donation record
  // => 4. Updates donor stats (1 donation, 100k lifetime)
  // => 5. Sends receipt email

  // => Display result of first donation
  print('Result: ${result1.success ? 'SUCCESS' : 'FAILED'}');
  // => Should print SUCCESS (mock payment always succeeds)
  print('Donation ID: ${result1.donationId}');
  // => Shows donation record ID (DON-xxxxx)
  print('Transaction ID: ${result1.transactionId}');
  // => Shows payment transaction ID (TXN-xxxxx)

  // Second donation from same donor
  // => Scenario: Existing donor makes another donation
  await service.processDonation(
    donorEmail: 'ahmad@example.com',  // => Same email as first donation
    donorName: 'Ahmad',               // => Name not used (donor exists)
    amount: 150000.0,                 // => 150k IDR donation
    paymentMethod: 'bank_transfer',   // => Different payment method
  );
  // => This triggers workflow again:
  // => 1. Finds existing donor (no creation this time)
  // => 2. Processes payment (150k)
  // => 3. Saves second donation record
  // => 4. Updates donor stats (2 donations, 250k lifetime)
  // => 5. Sends receipt email
  // => Result not captured (demonstrate fire-and-forget usage)

  // Get donor statistics
  // => Query updated donor stats after both donations
  DonorStats stats = await service.getDonorStatistics('ahmad@example.com');
  // => Retrieves aggregate stats from donor record
  print('Donor Statistics:');
  print('  Total donations: ${stats.totalDonations}');
  // => Should print: 2 (first donation + second donation)
  print('  Lifetime amount: Rp${stats.lifetimeAmount}');
  // => Should print: Rp250000.0 (100k + 150k)
  // => Stats updated by processDonation workflow (step 4)
}
// => Output demonstrates service layer orchestrating complex workflow
// => Service coordinates multiple repositories and external services
// => Maintains data consistency across operations
```

**Service Layer Responsibilities**:

| Responsibility         | Description                         | Example                           |
| ---------------------- | ----------------------------------- | --------------------------------- |
| Business logic         | Orchestrate complex operations      | Multi-step donation flow          |
| Transaction management | Ensure atomicity across operations  | Rollback on payment failure       |
| Validation             | Enforce business rules              | Minimum donation amount           |
| Coordination           | Call multiple repositories/services | Update donor stats after donation |
| Error handling         | Handle and transform errors         | Convert payment errors to result  |

**Key Takeaway**: Use service layer to orchestrate business logic across repositories and external services. Service coordinates multi-step operations, enforces business rules, and handles transactions. Keeps controllers/UI thin.

**Why It Matters**: Business logic scattered across controllers creates duplication and inconsistency. Service layer centralizes business rules, enabling reuse across different entry points (REST API, GraphQL, CLI). Transaction management ensures data consistency (donor stats match donations). In Flutter, service layer separates business logic from UI, enabling testing without widgets. For servers, service layer enforces authorization and business rules consistently.

**Common Pitfalls**: Service layer doing data access directly (bypass repository pattern). Services calling other services creating tight coupling. Missing transaction rollback on partial failure. Service layer containing UI logic (keep UI-agnostic).

---

### Example 73: Clean Architecture Principles

Applying clean architecture for maintainable, testable, framework-independent business logic.

```dart
// === DOMAIN LAYER (innermost) ===
// Business entities and interfaces (no dependencies)

class Donation {
  final String id;
  final String donorId;
  final double amount;
  final DateTime timestamp;

  Donation({
    required this.id,
    required this.donorId,
    required this.amount,
    required this.timestamp,
  });
}

// Repository interface (domain defines contract)
abstract class DonationRepositoryInterface {
  Future<void> save(Donation donation);
  Future<List<Donation>> getAllByDonor(String donorId);
}

// Use case (business logic)
class CreateDonationUseCase {
  final DonationRepositoryInterface repository;

  CreateDonationUseCase(this.repository);

  Future<CreateDonationResult> execute({
    required String donorId,
    required double amount,
  }) async {
    // Business rules validation
    if (amount < 10000) {
      return CreateDonationResult.failure('Minimum donation: Rp10,000');
    }

    Donation donation = Donation(
      id: 'DON-${DateTime.now().millisecondsSinceEpoch}',
      donorId: donorId,
      amount: amount,
      timestamp: DateTime.now(),
    );

    await repository.save(donation);

    return CreateDonationResult.success(donation);
  }
}

class CreateDonationResult {
  final bool success;
  final Donation? donation;
  final String? error;

  CreateDonationResult._({
    required this.success,
    this.donation,
    this.error,
  });

  factory CreateDonationResult.success(Donation donation) =>
      CreateDonationResult._(success: true, donation: donation);

  factory CreateDonationResult.failure(String error) =>
      CreateDonationResult._(success: false, error: error);
}

// === DATA LAYER (outermost) ===
// Implementation details (database, API, file system)

class InMemoryDonationRepository implements DonationRepositoryInterface {
  final Map<String, Donation> _storage = {};

  @override
  Future<void> save(Donation donation) async {
    _storage[donation.id] = donation;
    print('[REPOSITORY] Saved: ${donation.id}');
  }

  @override
  Future<List<Donation>> getAllByDonor(String donorId) async {
    return _storage.values
        .where((d) => d.donorId == donorId)
        .toList();
  }
}

// === PRESENTATION LAYER (outermost) ===
// UI, controllers, framework-specific code

class DonationController {
  final CreateDonationUseCase createDonationUseCase;

  DonationController(this.createDonationUseCase);

  Future<void> handleDonationRequest(String donorId, double amount) async {
    print('\n[CONTROLLER] Handling donation request...');

    CreateDonationResult result = await createDonationUseCase.execute(
      donorId: donorId,
      amount: amount,
    );

    if (result.success) {
      print('[CONTROLLER] Success: ${result.donation!.id}');
      _showSuccessMessage(result.donation!);
    } else {
      print('[CONTROLLER] Failure: ${result.error}');
      _showErrorMessage(result.error!);
    }
  }

  void _showSuccessMessage(Donation donation) {
    print('   UI: "Donation successful! ID: ${donation.id}"');
  }

  void _showErrorMessage(String error) {
    print('   UI: "Error: $error"');
  }
}

// Dependency injection (wiring)
class DependencyContainer {
  // Repositories
  late final DonationRepositoryInterface donationRepository;

  // Use cases
  late final CreateDonationUseCase createDonationUseCase;

  // Controllers
  late final DonationController donationController;

  DependencyContainer() {
    _setupDependencies();
  }

  void _setupDependencies() {
    // Data layer
    donationRepository = InMemoryDonationRepository();

    // Domain layer (use cases)
    createDonationUseCase = CreateDonationUseCase(donationRepository);

    // Presentation layer
    donationController = DonationController(createDonationUseCase);
  }
}

void main() async {
  print('=== Clean Architecture Demo ===\n');

  // Setup dependencies
  DependencyContainer container = DependencyContainer();

  // Valid donation
  await container.donationController.handleDonationRequest(
    'DONOR-001',
    100000.0,
  );

  // Invalid donation (below minimum)
  await container.donationController.handleDonationRequest(
    'DONOR-002',
    5000.0,
  );

  print('\n=== Architecture Layers ===');
  print('Domain (Core):');
  print('  ✅ Entities (Donation)');
  print('  ✅ Use Cases (CreateDonationUseCase)');
  print('  ✅ Repository Interfaces');
  print('  ❌ No framework dependencies');
  print('');
  print('Data (Infrastructure):');
  print('  ✅ Repository Implementations');
  print('  ✅ Database/API clients');
  print('  ➡️  Depends on Domain (implements interfaces)');
  print('');
  print('Presentation (UI):');
  print('  ✅ Controllers');
  print('  ✅ Views/Widgets');
  print('  ➡️  Depends on Domain (calls use cases)');
  print('');
  print('Dependency Flow: Presentation → Domain ← Data');
  print('(All depend on Domain, Domain depends on nothing)');
}
```

**Clean Architecture Layers**:

| Layer        | Contents                        | Dependencies | Examples                        |
| ------------ | ------------------------------- | ------------ | ------------------------------- |
| Domain       | Entities, use cases, interfaces | None         | Donation, CreateDonationUseCase |
| Data         | Repository implementations      | Domain       | InMemoryDonationRepository      |
| Presentation | Controllers, UI                 | Domain       | DonationController              |

**Dependency Rule**: Inner layers never depend on outer layers. Domain has zero dependencies.

**Key Takeaway**: Organize code in layers with Domain at center (no dependencies). Data and Presentation depend on Domain. Use cases orchestrate business logic. Repositories implement domain interfaces. Controllers call use cases.

**Why It Matters**: Framework coupling makes code untestable and fragile. Clean architecture isolates business logic from frameworks (Flutter, HTTP, databases). Domain layer is pure Dart—no imports except standard library. Enables testing business logic without UI or database. Swapping frameworks doesn't require changing business rules. In Flutter, clean architecture enables widget-free testing. For servers, enables framework migration (Express → Fastify) without changing business logic.

**Common Pitfalls**: Domain layer importing framework packages. Use cases calling repositories directly (bypass interfaces). Presentation layer containing business logic. Not using dependency injection (layers tightly coupled). Overcomplicated for simple CRUD apps.

---

### Example 74: Event-Driven Architecture with Streams

Using streams for event-driven communication between components, enabling loose coupling and reactive updates.

```dart
import 'dart:async';

// Events
abstract class DonationEvent {}

class DonationCreatedEvent implements DonationEvent {
  final String id;
  final String donorId;
  final double amount;
  final DateTime timestamp;

  DonationCreatedEvent(this.id, this.donorId, this.amount, this.timestamp);
}

class PaymentProcessedEvent implements DonationEvent {
  final String donationId;
  final String transactionId;

  PaymentProcessedEvent(this.donationId, this.transactionId);
}

class DonationCanceledEvent implements DonationEvent {
  final String id;
  final String reason;

  DonationCanceledEvent(this.id, this.reason);
}

// Event bus (pub/sub)
class EventBus {
  final StreamController<DonationEvent> _controller =
      StreamController<DonationEvent>.broadcast();
                                        // => Broadcast: multiple listeners

  Stream<DonationEvent> get stream => _controller.stream;

  void publish(DonationEvent event) {   // => Publish: emit event
    print('[EVENT BUS] Publishing: ${event.runtimeType}');
    _controller.add(event);
  }

  void dispose() {
    _controller.close();
  }
}

// Event listeners (subscribers)
class EmailNotificationService {
  final EventBus eventBus;

  EmailNotificationService(this.eventBus) {
    // Subscribe to donation created events
    eventBus.stream
        .where((event) => event is DonationCreatedEvent)
        .listen((event) {
      DonationCreatedEvent e = event as DonationCreatedEvent;
      _sendReceiptEmail(e.donorId, e.amount);
    });

    // Subscribe to payment processed events
    eventBus.stream
        .where((event) => event is PaymentProcessedEvent)
        .listen((event) {
      PaymentProcessedEvent e = event as PaymentProcessedEvent;
      _sendConfirmationEmail(e.transactionId);
    });
  }

  void _sendReceiptEmail(String donorId, double amount) {
    print('  [EMAIL] Sending receipt to $donorId for Rp$amount');
  }

  void _sendConfirmationEmail(String transactionId) {
    print('  [EMAIL] Sending confirmation for $transactionId');
  }
}

class AnalyticsService {
  final EventBus eventBus;

  AnalyticsService(this.eventBus) {
    eventBus.stream.listen((event) {    // => Subscribe: to all events
      _trackEvent(event);
    });
  }

  void _trackEvent(DonationEvent event) {
    print('  [ANALYTICS] Tracked: ${event.runtimeType}');
  }
}

class DonationStatisticsService {
  int _totalDonations = 0;
  double _totalAmount = 0.0;
  final EventBus eventBus;

  DonationStatisticsService(this.eventBus) {
    eventBus.stream
        .where((event) => event is DonationCreatedEvent)
        .listen((event) {
      DonationCreatedEvent e = event as DonationCreatedEvent;
      _totalDonations++;
      _totalAmount += e.amount;
      print('  [STATS] Total donations: $_totalDonations, Total amount: Rp$_totalAmount');
    });
  }

  Map<String, dynamic> getStatistics() => {
    'totalDonations': _totalDonations,
    'totalAmount': _totalAmount,
  };
}

// Domain service (publisher)
class DonationService {
  final EventBus eventBus;

  DonationService(this.eventBus);

  Future<void> createDonation(String donorId, double amount) async {
    print('\n[SERVICE] Creating donation...');

    String id = 'DON-${DateTime.now().millisecondsSinceEpoch}';

    // Publish event
    eventBus.publish(DonationCreatedEvent(
      id,
      donorId,
      amount,
      DateTime.now(),
    ));

    // Simulate payment processing
    await Future.delayed(Duration(milliseconds: 100));

    eventBus.publish(PaymentProcessedEvent(
      id,
      'TXN-${DateTime.now().millisecondsSinceEpoch}',
    ));

    print('[SERVICE] Donation created: $id\n');
  }

  Future<void> cancelDonation(String id, String reason) async {
    print('\n[SERVICE] Canceling donation...');

    eventBus.publish(DonationCanceledEvent(id, reason));

    print('[SERVICE] Donation canceled: $id\n');
  }
}

void main() async {
  print('=== Event-Driven Architecture ===\n');

  // Setup event bus
  EventBus eventBus = EventBus();

  // Setup subscribers (automatically listen to events)
  EmailNotificationService emailService = EmailNotificationService(eventBus);
  AnalyticsService analyticsService = AnalyticsService(eventBus);
  DonationStatisticsService statsService = DonationStatisticsService(eventBus);

  // Setup publisher
  DonationService donationService = DonationService(eventBus);

  // Create donations (triggers events)
  await donationService.createDonation('DONOR-001', 100000.0);
  await donationService.createDonation('DONOR-002', 200000.0);
  await donationService.cancelDonation('DON-123', 'Duplicate payment');

  // Check statistics
  print('Final Statistics: ${statsService.getStatistics()}');

  print('\n=== Event-Driven Benefits ===');
  print('✅ Loose coupling: components don\'t know about each other');
  print('✅ Scalability: add new subscribers without modifying publishers');
  print('✅ Async processing: events processed independently');
  print('✅ Audit trail: all events can be logged/stored');

  // Cleanup
  eventBus.dispose();
}
```

**Event-Driven Patterns**:

| Pattern        | Implementation                  | Use Case                         |
| -------------- | ------------------------------- | -------------------------------- |
| Event bus      | StreamController.broadcast      | Cross-component communication    |
| Pub/Sub        | Event publishing/subscribing    | Loose coupling                   |
| Event sourcing | Store events as source of truth | Audit trail, time travel         |
| CQRS           | Separate read/write models      | Scale reads/writes independently |

**Key Takeaway**: Use event bus for loose coupling between components. Publishers emit events without knowing subscribers. Subscribers react to events independently. Enables async processing and auditability.

**Why It Matters**: Tight coupling makes components fragile—adding email notification requires modifying donation logic. Event-driven architecture decouples publishers from subscribers. Adding analytics service doesn't require changing donation service. Events provide audit trail (what happened when). In Flutter, event-driven updates enable reactive UI (widget rebuilds on events). For microservices, events enable cross-service communication without direct dependencies.

**Common Pitfalls**: Event bus as global variable (use dependency injection). Not disposing streams (memory leaks). Events carrying too much data (copy instead of reference). Circular event dependencies (event A triggers event B which triggers event A). Missing error handling in subscribers.

---

### Example 75: Advanced Testing Strategies

Implementing comprehensive testing strategies including unit, widget, integration, and property-based tests.

```dart
// === Code under test ===
// => Production class demonstrating testable design
class ZakatCalculator {
  // => Calculates Zakat (Islamic alms-giving obligation)
  // => wealth: total wealth to evaluate for Zakat
  // => nisabValue: minimum threshold for Zakat obligation (default: 8.5M IDR)
  double calculate(double wealth, {double nisabValue = 8500000.0}) {
    // => Validation: reject negative wealth values
    if (wealth < 0) {
      // => Throws ArgumentError for invalid input
      throw ArgumentError('Wealth cannot be negative');
      // => This allows tests to verify error handling behavior
    }

    // => Business rule: no Zakat due if wealth below nisab
    if (wealth < nisabValue) {
      return 0.0;                       // => Below nisab: no Zakat due
      // => This is a critical boundary condition for testing
    }

    // => Business rule: Zakat is 2.5% of zakatable wealth
    return wealth * 0.025;              // => 2.5% Zakat rate
    // => This formula is the core business logic to verify
  }

  // => Helper method: determines if Zakat is due
  // => Returns true if wealth >= nisab threshold
  bool isZakatDue(double wealth, {double nisabValue = 8500000.0}) {
    return wealth >= nisabValue;
    // => Simple comparison enables boundary testing
    // => Useful for UI logic (showing "Zakat due" indicator)
  }
}

// === Test Framework (simulated) ===
// => Custom test runner demonstrating testing infrastructure
// => In production, use package:test or package:flutter_test
class TestSuite {
  final String description;              // => Test suite name
  final List<Test> tests = [];           // => Collects all test results
  int passed = 0;                        // => Tracks successful tests
  int failed = 0;                        // => Tracks failed tests

  // => Constructor initializes test suite with descriptive name
  TestSuite(this.description);

  // => Core method: runs individual test and captures result
  void test(String description, void Function() testFn) {
    try {
      testFn();                          // => Execute test function
      // => If no exception thrown, test passes
      tests.add(Test(description, true));
      passed++;                          // => Increment pass counter
      print('  ✅ $description');        // => Visual feedback for passing test
    } catch (e) {
      // => If exception thrown, test fails
      tests.add(Test(description, false, error: e.toString()));
      failed++;                          // => Increment fail counter
      print('  ❌ $description: $e');   // => Show error message
      // => Captures exception details for debugging
    }
  }

  // => Prints test suite header
  void run() {
    print('\n=== $description ===');
    print('');
    // => Provides visual separation between test suites
  }

  // => Prints test execution summary
  void summary() {
    print('');
    print('Passed: $passed, Failed: $failed');
    // => Shows aggregate results for suite
    // => Helps identify test suite health at a glance
  }
}

// => Data class representing single test result
class Test {
  final String description;              // => Test name
  final bool passed;                     // => Pass/fail status
  final String? error;                   // => Optional error message

  // => Constructor captures test outcome
  Test(this.description, this.passed, {this.error});
  // => Error parameter only populated for failed tests
}

// => Assertion function: verifies actual value matches expectation
void expect(dynamic actual, Matcher matcher) {
  // => Uses matcher pattern for flexible comparisons
  if (!matcher.matches(actual)) {
    // => If match fails, throw descriptive error
    throw AssertionError('Expected: ${matcher.description}, Actual: $actual');
    // => Error message shows expected vs actual for debugging
  }
  // => If match succeeds, returns normally (test passes)
}

// => Abstract base class for matcher pattern
abstract class Matcher {
  // => Each matcher implements custom comparison logic
  bool matches(dynamic value);
  // => Each matcher provides description for error messages
  String get description;
}

// => Matcher for equality comparison (==)
class Equals implements Matcher {
  final dynamic expected;                // => Stores expected value

  // => Constructor captures expected value for comparison
  Equals(this.expected);

  @override
  // => Uses Dart equality operator for comparison
  bool matches(dynamic value) => value == expected;
  // => Works for primitives, strings, and objects with == override

  @override
  // => Description for error messages
  String get description => '$expected';
  // => Shows what value was expected
}

// => Matcher for numeric comparisons (>)
class GreaterThan implements Matcher {
  final num threshold;                   // => Stores comparison threshold

  // => Constructor captures threshold value
  GreaterThan(this.threshold);

  @override
  // => Compares numeric values
  bool matches(dynamic value) => value > threshold;
  // => Returns true if value exceeds threshold

  @override
  // => Description for error messages
  String get description => 'greater than $threshold';
  // => Shows threshold for failed comparisons
}

// => Matcher for exception-throwing behavior
class Throws implements Matcher {
  @override
  // => Executes function and checks if exception thrown
  bool matches(dynamic value) {
    try {
      if (value is Function) {
        value();                         // => Execute function
        // => If we reach here, no exception thrown
      }
      return false;                      // => Test fails (no exception)
    } catch (e) {
      return true;                       // => Test passes (exception caught)
      // => Any exception type counts as match
    }
  }

  @override
  // => Description for error messages
  String get description => 'throws exception';
  // => Indicates test expected exception to be thrown
}

// => Factory functions for readable test syntax
Matcher equals(dynamic value) => Equals(value);
// => Usage: expect(result, equals(42))
Matcher greaterThan(num value) => GreaterThan(value);
// => Usage: expect(score, greaterThan(50))
Matcher get throwsException => Throws();
// => Usage: expect(() => risky(), throwsException)

// === Tests ===
// => Main function orchestrates all test suites
void main() {
  // => System under test: instantiate calculator
  ZakatCalculator calculator = ZakatCalculator();
  // => Single instance used across all tests (stateless class)

  // Unit tests (test individual methods)
  // => Unit tests verify individual method behavior in isolation
  TestSuite unitTests = TestSuite('Unit Tests');
  unitTests.run();                       // => Print suite header

  // => Test 1: Normal case - wealth above threshold
  unitTests.test('Calculate Zakat for wealth above nisab', () {
    double result = calculator.calculate(10000000.0);
    // => Input: 10M IDR (above 8.5M nisab)
    expect(result, equals(250000.0));   // => 10M * 0.025 = 250k
    // => Verifies core business logic calculation
  });

  // => Test 2: Boundary case - wealth below threshold
  unitTests.test('Return 0 for wealth below nisab', () {
    double result = calculator.calculate(5000000.0);
    // => Input: 5M IDR (below 8.5M nisab)
    expect(result, equals(0.0));
    // => Verifies no Zakat charged below nisab
    // => Critical business rule enforcement
  });

  // => Test 3: Error case - invalid input
  unitTests.test('Throw error for negative wealth', () {
    // => Pass function closure to test exception throwing
    expect(() => calculator.calculate(-1000.0), throwsException);
    // => Verifies input validation works correctly
    // => ArgumentError should be thrown for negative values
  });

  // => Test 4: Helper method - positive case
  unitTests.test('Is Zakat due for wealth above nisab', () {
    bool result = calculator.isZakatDue(10000000.0);
    // => Input: 10M IDR (above threshold)
    expect(result, equals(true));
    // => Verifies helper correctly identifies Zakat obligation
  });

  // => Test 5: Helper method - negative case
  unitTests.test('Is Zakat not due for wealth below nisab', () {
    bool result = calculator.isZakatDue(5000000.0);
    // => Input: 5M IDR (below threshold)
    expect(result, equals(false));
    // => Verifies helper correctly identifies no obligation
  });

  unitTests.summary();                   // => Print pass/fail counts

  // Boundary tests (test edge cases)
  // => Boundary tests verify behavior at thresholds and limits
  TestSuite boundaryTests = TestSuite('Boundary Tests');
  boundaryTests.run();                   // => Print suite header

  // => Test 6: Exact boundary condition
  boundaryTests.test('Zakat for wealth exactly at nisab', () {
    double result = calculator.calculate(8500000.0);
    // => Input: exactly 8.5M IDR (nisab threshold)
    expect(result, equals(212500.0));   // => At boundary: 8.5M * 0.025 = 212.5k
    // => Critical test: wealth >= nisab should trigger Zakat
    // => Verifies >= comparison, not > comparison
  });

  // => Test 7: Just below boundary
  boundaryTests.test('Zakat for wealth just below nisab', () {
    double result = calculator.calculate(8499999.0);
    // => Input: 8,499,999 IDR (1 rupiah below nisab)
    expect(result, equals(0.0));
    // => Verifies strict threshold enforcement
    // => Even 1 rupiah below = no Zakat
  });

  // => Test 8: Zero edge case
  boundaryTests.test('Zakat for zero wealth', () {
    double result = calculator.calculate(0.0);
    // => Input: 0 IDR (minimum valid wealth)
    expect(result, equals(0.0));
    // => Verifies zero wealth handled correctly
    // => Not negative, but below nisab
  });

  boundaryTests.summary();               // => Print pass/fail counts

  // Property-based tests (test properties hold for random inputs)
  // => Property-based testing verifies invariants across many inputs
  TestSuite propertyTests = TestSuite('Property-Based Tests');
  propertyTests.run();                   // => Print suite header

  // => Test 9: Invariant - Zakat rate is always 2.5%
  propertyTests.test('Zakat is always 2.5% for wealth above nisab', () {
    // => Test data: various wealth values above nisab
    List<double> wealthValues = [9000000.0, 15000000.0, 50000000.0, 100000000.0];
    // => Covers range: just above nisab to very high wealth

    // => Verify property holds for all test values
    for (double wealth in wealthValues) {
      double zakat = calculator.calculate(wealth);
      // => Calculate expected value independently
      double expected = wealth * 0.025;
      expect(zakat, equals(expected));
      // => Property: Zakat = wealth * 2.5% for all wealth >= nisab
      // => Verifies formula consistency across wealth spectrum
    }
    // => If loop completes, property holds for all inputs
  });

  // => Test 10: Invariant - Zero Zakat below nisab
  propertyTests.test('Zakat is always 0 for wealth below nisab', () {
    // => Test data: various wealth values below nisab
    List<double> wealthValues = [0.0, 1000000.0, 5000000.0, 8000000.0];
    // => Covers range: zero to just below nisab

    // => Verify property holds for all test values
    for (double wealth in wealthValues) {
      double zakat = calculator.calculate(wealth);
      // => Calculate actual Zakat
      expect(zakat, equals(0.0));
      // => Property: Zakat = 0 for all wealth < nisab
      // => Verifies threshold enforcement is consistent
    }
  });

  // => Test 11: Invariant - Zakat never exceeds wealth
  propertyTests.test('Calculated Zakat never exceeds wealth', () {
    // => Test data: various wealth values
    List<double> wealthValues = [10000000.0, 20000000.0, 100000000.0];
    // => Different magnitudes to verify rate consistency

    // => Verify property holds for all test values
    for (double wealth in wealthValues) {
      double zakat = calculator.calculate(wealth);
      // => Calculate actual Zakat
      if (zakat > wealth) {
        // => Property violation: Zakat should never exceed 100% of wealth
        throw AssertionError('Zakat $zakat exceeds wealth $wealth');
        // => This would indicate incorrect formula (rate > 1.0)
      }
      // => Since rate is 0.025 (2.5%), this should always pass
    }
    // => Verifies sanity of business logic
  });

  propertyTests.summary();               // => Print pass/fail counts

  // Parameterized tests
  // => Parameterized testing runs same test logic with different inputs
  TestSuite parameterizedTests = TestSuite('Parameterized Tests');
  parameterizedTests.run();              // => Print suite header

  // => Test data: table of inputs and expected outputs
  List<Map<String, dynamic>> testCases = [
    // => Each map contains input wealth and expected Zakat
    {'wealth': 10000000.0, 'expected': 250000.0},   // => 10M → 250k
    {'wealth': 20000000.0, 'expected': 500000.0},   // => 20M → 500k
    {'wealth': 15000000.0, 'expected': 375000.0},   // => 15M → 375k
    {'wealth': 8500000.0, 'expected': 212500.0},    // => 8.5M → 212.5k (boundary)
  ];
  // => Data-driven testing: separates test data from test logic

  // => Test 12-15: Run same test with different parameters
  for (Map<String, dynamic> testCase in testCases) {
    // => Create one test per test case
    parameterizedTests.test(
      'Calculate Zakat for ${testCase['wealth']}',
      // => Dynamic test name includes input value
      () {
        double result = calculator.calculate(testCase['wealth']);
        // => Extract wealth from test case map
        expect(result, equals(testCase['expected']));
        // => Extract expected result from test case map
        // => Verifies calculation matches expected output
      },
    );
    // => Loop generates 4 tests with descriptive names
  }

  parameterizedTests.summary();          // => Print pass/fail counts

  // Performance tests
  // => Performance tests verify execution speed meets requirements
  print('\n=== Performance Tests ===\n');
  // => Performance testing not integrated with test framework

  // => Measure execution time for many iterations
  Stopwatch sw = Stopwatch()..start();
  // => Start high-precision timer
  for (int i = 0; i < 100000; i++) {
    // => Run calculation 100,000 times
    calculator.calculate(10000000.0);
    // => Same input each iteration (measures pure calculation speed)
  }
  sw.stop();                             // => Stop timer

  // => Report performance metrics
  print('  100,000 calculations: ${sw.elapsedMilliseconds}ms');
  // => Total elapsed time in milliseconds
  print('  Average: ${sw.elapsedMicroseconds / 100000}μs per calculation');
  // => Average time per calculation in microseconds
  // => Helps identify performance regressions

  print('\n=== Testing Strategies Summary ===');
  print('✅ Unit tests: Test individual methods');
  print('✅ Boundary tests: Test edge cases');
  print('✅ Property-based tests: Test invariants');
  print('✅ Parameterized tests: Data-driven testing');
  print('✅ Performance tests: Measure execution time');
}
```

**Testing Pyramid**:

| Level       | Count | Speed  | Scope               | Example                     |
| ----------- | ----- | ------ | ------------------- | --------------------------- |
| Unit        | Many  | Fast   | Single method       | ZakatCalculator.calculate() |
| Integration | Some  | Medium | Multiple components | Service + Repository        |
| End-to-end  | Few   | Slow   | Full application    | UI → API → Database         |

**Key Takeaway**: Write comprehensive tests at multiple levels. Unit tests for individual methods. Boundary tests for edge cases. Property-based tests for invariants. Parameterized tests for data-driven scenarios. Performance tests for benchmarks.

**Why It Matters**: Untested code breaks silently in production. Unit tests catch regressions during development. Boundary tests find edge-case bugs (exactly at nisab, zero wealth). Property-based tests verify invariants hold for all inputs (Zakat ≤ wealth). In Flutter, widget tests validate UI behavior without running app. For libraries, comprehensive tests enable safe refactoring.

**Common Pitfalls**: Testing implementation details instead of behavior. Not testing edge cases (null, zero, negative). Flaky tests (depend on time/random values). Slow tests (integration tests for unit test cases). Missing test coverage metrics.

---

## Summary

You've completed **25 comprehensive advanced examples** covering **20% more of Dart** (75-95% total coverage):

**Advanced Async and Isolates** (Examples 51-55):

- Isolates for true parallelism with separate memory
- Bidirectional isolate communication
- Compute function for simplified one-shot isolates
- Stream.periodic for recurring scheduled tasks
- Future.any for racing multiple async operations

**Design Patterns** (Examples 56-58):

- Singleton pattern for single instance guarantee
- Factory pattern for polymorphic object creation
- Observer pattern with stream-based implementation

**Testing** (Examples 59-60):

- Unit testing with package:test framework
- Integration testing with async operations
- Test organization and assertions

**Performance and Optimization** (Examples 61-66):

- Lazy initialization for deferred expensive operations
- Memoization and caching for result reuse
- Memory management and object pooling
- String optimization (StringBuffer vs concatenation)
- Collection performance (Set vs List, preallocated capacity)
- Benchmarking and profiling for scientific measurement

**Production Patterns** (Examples 67-70):

- Production logging with levels and structured data
- Error handling and monitoring integration
- Configuration management across environments
- Dependency injection for testable code

**Architecture Patterns** (Examples 71-75):

- Repository pattern for data access abstraction
- Service layer for business logic orchestration
- Clean architecture with layered dependencies
- Event-driven architecture with streams
- Advanced testing strategies (unit, boundary, property-based, parameterized, performance)

**Production-Ready Patterns Covered**:

- ✅ Isolates for CPU-intensive parallel processing
- ✅ Design patterns for maintainable code
- ✅ Comprehensive testing at multiple levels
- ✅ Performance optimization techniques
- ✅ Production logging and monitoring
- ✅ Error handling with circuit breakers and retries
- ✅ Configuration management for multiple environments
- ✅ Dependency injection for loose coupling
- ✅ Repository pattern for data layer abstraction
- ✅ Clean architecture for framework independence
- ✅ Event-driven architecture for reactive systems

## Complete Learning Path

You've mastered **95% of Dart** through **75 heavily annotated examples**:

**Beginner (Examples 1-25)**: 40% coverage

- Variables, types, control flow
- Functions, classes, collections
- Basic OOP (constructors, inheritance)

**Intermediate (Examples 26-50)**: 35% additional coverage (40-75% total)

- Async programming (Futures, Streams)
- Advanced OOP (mixins, generics, extension methods)
- Advanced features (callable classes, records, operator overloading)

**Advanced (Examples 51-75)**: 20% additional coverage (75-95% total)

- Isolates and parallelism
- Design patterns and architecture
- Testing and quality assurance
- Performance optimization
- Production patterns

**Total Coverage**: 95% of Dart language and standard library

**What's Not Covered** (remaining 5%):

- Extremely rare language features
- Deprecated APIs
- Platform-specific edge cases
- Experimental features

**Next Steps**:

- **Build real applications**: Apply patterns in practice
- **Explore Flutter framework**: UI development with Dart
- **Study Dart package ecosystem**: pub.dev for libraries
- **Contribute to open source**: Dart/Flutter projects
- **Master production deployment**: CI/CD, monitoring, scaling

**Advanced Mastery Achieved**: You now possess expert-level Dart knowledge for building production-grade applications with clean architecture, comprehensive testing, performance optimization, and industry-standard patterns.

All examples maintain 1.0-2.25 annotation density with Islamic finance contexts for real-world relevance.
