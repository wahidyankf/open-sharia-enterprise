/// Expense summary screen — P&L reporting with date range and currency filter.
///
/// Fetches the P&L report via [reports_api.getPLReport] for the selected
/// date range and currency. Displays income total, expense total, net P&L,
/// and a category breakdown. Per-currency grouping: no cross-currency totals.
/// Wrapped in [AppShell].
library;

import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:demo_fe_dart_flutter/core/api/reports_api.dart' as reports_api;
import 'package:demo_fe_dart_flutter/core/models/models.dart';
import 'package:demo_fe_dart_flutter/widgets/app_shell.dart';

// ---------------------------------------------------------------------------
// Provider
// ---------------------------------------------------------------------------

class _PLParams {
  const _PLParams({
    required this.startDate,
    required this.endDate,
    required this.currency,
  });

  final String startDate;
  final String endDate;
  final String currency;

  @override
  bool operator ==(Object other) =>
      other is _PLParams &&
      other.startDate == startDate &&
      other.endDate == endDate &&
      other.currency == currency;

  @override
  int get hashCode => Object.hash(startDate, endDate, currency);
}

final _plReportProvider = FutureProvider.family<PLReport, _PLParams>(
  (ref, params) => reports_api.getPLReport(
    startDate: params.startDate,
    endDate: params.endDate,
    currency: params.currency,
  ),
);

// ---------------------------------------------------------------------------
// Screen widget
// ---------------------------------------------------------------------------

class ExpenseSummaryScreen extends ConsumerStatefulWidget {
  const ExpenseSummaryScreen({super.key});

  @override
  ConsumerState<ExpenseSummaryScreen> createState() =>
      _ExpenseSummaryScreenState();
}

class _ExpenseSummaryScreenState extends ConsumerState<ExpenseSummaryScreen> {
  static const List<String> _currencies = ['USD', 'EUR', 'GBP', 'IDR', 'MYR'];

  late DateTime _startDate;
  late DateTime _endDate;
  String _currency = 'USD';

  @override
  void initState() {
    super.initState();
    final now = DateTime.now();
    _startDate = DateTime(now.year, now.month, 1);
    _endDate = now;
  }

  String _fmt(DateTime d) =>
      '${d.year}-${d.month.toString().padLeft(2, '0')}-${d.day.toString().padLeft(2, '0')}';

  _PLParams get _params => _PLParams(
    startDate: _fmt(_startDate),
    endDate: _fmt(_endDate),
    currency: _currency,
  );

  @override
  Widget build(BuildContext context) {
    final reportAsync = ref.watch(_plReportProvider(_params));

    return AppShell(
      child: ListView(
        padding: const EdgeInsets.all(24),
        children: [
          Text(
            'Expense Summary',
            style: Theme.of(context).textTheme.headlineMedium,
          ),
          const SizedBox(height: 24),
          _FilterBar(
            startDate: _startDate,
            endDate: _endDate,
            currency: _currency,
            currencies: _currencies,
            onStartDateChanged: (d) => setState(() => _startDate = d),
            onEndDateChanged: (d) => setState(() => _endDate = d),
            onCurrencyChanged: (c) => setState(() => _currency = c),
          ),
          const SizedBox(height: 24),
          reportAsync.when(
            loading: () => const Center(child: CircularProgressIndicator()),
            error: (e, _) => _ErrorCard(error: e),
            data: (report) => _ReportContent(report: report),
          ),
        ],
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// Filter bar
// ---------------------------------------------------------------------------

class _FilterBar extends StatelessWidget {
  const _FilterBar({
    required this.startDate,
    required this.endDate,
    required this.currency,
    required this.currencies,
    required this.onStartDateChanged,
    required this.onEndDateChanged,
    required this.onCurrencyChanged,
  });

  final DateTime startDate;
  final DateTime endDate;
  final String currency;
  final List<String> currencies;
  final ValueChanged<DateTime> onStartDateChanged;
  final ValueChanged<DateTime> onEndDateChanged;
  final ValueChanged<String> onCurrencyChanged;

  String _fmt(DateTime d) =>
      '${d.year}-${d.month.toString().padLeft(2, '0')}-${d.day.toString().padLeft(2, '0')}';

  @override
  Widget build(BuildContext context) {
    return Wrap(
      spacing: 12,
      runSpacing: 12,
      crossAxisAlignment: WrapCrossAlignment.center,
      children: [
        _DateChip(
          label: 'From: ${_fmt(startDate)}',
          onTap: () async {
            final picked = await showDatePicker(
              context: context,
              initialDate: startDate,
              firstDate: DateTime(2000),
              lastDate: DateTime(2100),
            );
            if (picked != null) onStartDateChanged(picked);
          },
        ),
        _DateChip(
          label: 'To: ${_fmt(endDate)}',
          onTap: () async {
            final picked = await showDatePicker(
              context: context,
              initialDate: endDate,
              firstDate: DateTime(2000),
              lastDate: DateTime(2100),
            );
            if (picked != null) onEndDateChanged(picked);
          },
        ),
        DropdownButton<String>(
          value: currency,
          underline: const SizedBox(),
          items: currencies
              .map((c) => DropdownMenuItem(value: c, child: Text(c)))
              .toList(),
          onChanged: (c) {
            if (c != null) onCurrencyChanged(c);
          },
        ),
      ],
    );
  }
}

class _DateChip extends StatelessWidget {
  const _DateChip({required this.label, required this.onTap});

  final String label;
  final VoidCallback onTap;

  @override
  Widget build(BuildContext context) {
    return ActionChip(
      avatar: const Icon(Icons.calendar_today, size: 16),
      label: Text(label),
      onPressed: onTap,
    );
  }
}

// ---------------------------------------------------------------------------
// Report content
// ---------------------------------------------------------------------------

class _ReportContent extends StatelessWidget {
  const _ReportContent({required this.report});

  final PLReport report;

  @override
  Widget build(BuildContext context) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        _SummaryCards(report: report),
        const SizedBox(height: 24),
        if (report.categoryBreakdowns.isNotEmpty) ...[
          Text(
            'Category Breakdown',
            style: Theme.of(context).textTheme.titleMedium,
          ),
          const SizedBox(height: 12),
          _CategoryTable(
            breakdowns: report.categoryBreakdowns,
            currency: report.currency,
          ),
        ],
      ],
    );
  }
}

class _SummaryCards extends StatelessWidget {
  const _SummaryCards({required this.report});

  final PLReport report;

  @override
  Widget build(BuildContext context) {
    final net = report.netProfitLoss;
    final netColor = net >= 0 ? Colors.green.shade700 : Colors.red.shade700;
    final decimals = report.currency.toUpperCase() == 'IDR' ? 0 : 2;

    String fmt(double v) => '${v.toStringAsFixed(decimals)} ${report.currency}';

    return Wrap(
      spacing: 16,
      runSpacing: 16,
      children: [
        _SummaryCard(
          label: 'Total Income',
          value: fmt(report.totalIncome),
          icon: Icons.trending_up,
          color: Colors.green.shade700,
        ),
        _SummaryCard(
          label: 'Total Expenses',
          value: fmt(report.totalExpenses),
          icon: Icons.trending_down,
          color: Colors.red.shade700,
        ),
        _SummaryCard(
          label: 'Net P&L',
          value: fmt(net),
          icon: net >= 0 ? Icons.check_circle_outline : Icons.warning_outlined,
          color: netColor,
        ),
      ],
    );
  }
}

class _SummaryCard extends StatelessWidget {
  const _SummaryCard({
    required this.label,
    required this.value,
    required this.icon,
    required this.color,
  });

  final String label;
  final String value;
  final IconData icon;
  final Color color;

  @override
  Widget build(BuildContext context) {
    return Card(
      child: Padding(
        padding: const EdgeInsets.symmetric(horizontal: 20, vertical: 16),
        child: Row(
          mainAxisSize: MainAxisSize.min,
          children: [
            Icon(icon, color: color, size: 32),
            const SizedBox(width: 12),
            Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(label, style: Theme.of(context).textTheme.bodySmall),
                Text(
                  value,
                  style: Theme.of(context).textTheme.titleLarge?.copyWith(
                    color: color,
                    fontWeight: FontWeight.bold,
                  ),
                ),
              ],
            ),
          ],
        ),
      ),
    );
  }
}

class _CategoryTable extends StatelessWidget {
  const _CategoryTable({required this.breakdowns, required this.currency});

  final List<CategoryBreakdown> breakdowns;
  final String currency;

  @override
  Widget build(BuildContext context) {
    final decimals = currency.toUpperCase() == 'IDR' ? 0 : 2;

    return Card(
      child: SingleChildScrollView(
        scrollDirection: Axis.horizontal,
        child: DataTable(
          columns: const [
            DataColumn(label: Text('Category')),
            DataColumn(label: Text('Amount'), numeric: true),
            DataColumn(label: Text('Count'), numeric: true),
            DataColumn(label: Text('% Share'), numeric: true),
          ],
          rows: breakdowns
              .map(
                (b) => DataRow(
                  cells: [
                    DataCell(Text(b.category)),
                    DataCell(
                      Text(
                        '${b.totalAmount.toStringAsFixed(decimals)} $currency',
                      ),
                    ),
                    DataCell(Text('${b.expenseCount}')),
                    DataCell(Text('${b.percentage.toStringAsFixed(1)}%')),
                  ],
                ),
              )
              .toList(),
        ),
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// Error card
// ---------------------------------------------------------------------------

class _ErrorCard extends StatelessWidget {
  const _ErrorCard({required this.error});

  final Object error;

  @override
  Widget build(BuildContext context) {
    return Card(
      color: Colors.red.shade50,
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Row(
          children: [
            Icon(Icons.error_outline, color: Colors.red.shade700),
            const SizedBox(width: 12),
            Expanded(child: Text('$error')),
          ],
        ),
      ),
    );
  }
}
