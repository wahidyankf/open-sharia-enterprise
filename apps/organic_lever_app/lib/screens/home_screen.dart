import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import '../providers/hello_provider.dart';

class HomeScreen extends StatelessWidget {
  const HomeScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Organic Lever'),
        centerTitle: true,
      ),
      body: Center(
        child: Consumer<HelloProvider>(
          builder: (context, provider, child) {
            if (provider.isLoading) {
              return const CircularProgressIndicator();
            }

            if (provider.error != null) {
              return Column(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  const Icon(Icons.error, color: Colors.red, size: 48),
                  const SizedBox(height: 16),
                  Text(
                    'Error: ${provider.error}',
                    textAlign: TextAlign.center,
                  ),
                  const SizedBox(height: 16),
                  ElevatedButton(
                    onPressed: () => provider.fetchHello(),
                    child: const Text('Retry'),
                  ),
                ],
              );
            }

            if (provider.response != null) {
              return Column(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  const Icon(Icons.check_circle, color: Colors.green, size: 48),
                  const SizedBox(height: 16),
                  Text(
                    'API Response:',
                    style: Theme.of(context).textTheme.titleMedium,
                  ),
                  const SizedBox(height: 8),
                  Text(
                    provider.response!.message,
                    style: Theme.of(context).textTheme.headlineMedium,
                  ),
                ],
              );
            }

            return ElevatedButton(
              onPressed: () => provider.fetchHello(),
              child: const Text('Fetch Hello'),
            );
          },
        ),
      ),
    );
  }
}
