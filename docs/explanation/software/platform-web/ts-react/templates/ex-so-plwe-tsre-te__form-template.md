# React Form Template

Production-ready TypeScript form template with validation following OSE Platform conventions.

## Template

```typescript
import React, { useState, FormEvent, ChangeEvent } from 'react';

/**
 * Form data interface
 */
interface FormData {
  name: string;
  email: string;
  age: number;
  agreeToTerms: boolean;
}

/**
 * Form errors interface
 */
interface FormErrors {
  name?: string;
  email?: string;
  age?: string;
  agreeToTerms?: string;
}

/**
 * Form props
 */
interface FormComponentProps {
  onSubmit: (data: FormData) => Promise<void>;
  initialValues?: Partial<FormData>;
}

/**
 * FormComponent - Form with validation
 */
export const FormComponent: React.FC<FormComponentProps> = ({
  onSubmit,
  initialValues = {},
}) => {
  // Default values
  const defaultValues: FormData = {
    name: '',
    email: '',
    age: 0,
    agreeToTerms: false,
  };

  // Form state
  const [formData, setFormData] = useState<FormData>({
    ...defaultValues,
    ...initialValues,
  });

  // Error state
  const [errors, setErrors] = useState<FormErrors>({});

  // Submission state
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [submitError, setSubmitError] = useState<string | null>(null);
  const [submitSuccess, setSubmitSuccess] = useState(false);

  // Validation function
  const validate = (): FormErrors => {
    const newErrors: FormErrors = {};

    // Name validation
    if (!formData.name.trim()) {
      newErrors.name = 'Name is required';
    } else if (formData.name.length < 2) {
      newErrors.name = 'Name must be at least 2 characters';
    }

    // Email validation
    if (!formData.email.trim()) {
      newErrors.email = 'Email is required';
    } else if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(formData.email)) {
      newErrors.email = 'Email is invalid';
    }

    // Age validation
    if (formData.age <= 0) {
      newErrors.age = 'Age must be greater than 0';
    } else if (formData.age > 150) {
      newErrors.age = 'Age must be less than 150';
    }

    // Terms validation
    if (!formData.agreeToTerms) {
      newErrors.agreeToTerms = 'You must agree to terms';
    }

    return newErrors;
  };

  // Handle input change
  const handleChange = (e: ChangeEvent<HTMLInputElement>) => {
    const { name, value, type, checked } = e.target;

    setFormData(prev => ({
      ...prev,
      [name]: type === 'checkbox' ? checked : type === 'number' ? Number(value) : value,
    }));

    // Clear error for this field
    if (errors[name as keyof FormErrors]) {
      setErrors(prev => ({
        ...prev,
        [name]: undefined,
      }));
    }
  };

  // Handle form submission
  const handleSubmit = async (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault();

    // Validate
    const validationErrors = validate();
    if (Object.keys(validationErrors).length > 0) {
      setErrors(validationErrors);
      return;
    }

    // Submit
    setIsSubmitting(true);
    setSubmitError(null);
    setSubmitSuccess(false);

    try {
      await onSubmit(formData);
      setSubmitSuccess(true);
      // Reset form on success
      setFormData(defaultValues);
    } catch (error) {
      setSubmitError(error instanceof Error ? error.message : 'Submission failed');
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <form onSubmit={handleSubmit} className="form-container">
      {/* Name Field */}
      <div className="form-field">
        <label htmlFor="name">
          Name <span className="required">*</span>
        </label>
        <input
          id="name"
          name="name"
          type="text"
          value={formData.name}
          onChange={handleChange}
          className={errors.name ? 'error' : ''}
          disabled={isSubmitting}
        />
        {errors.name && <span className="error-message">{errors.name}</span>}
      </div>

      {/* Email Field */}
      <div className="form-field">
        <label htmlFor="email">
          Email <span className="required">*</span>
        </label>
        <input
          id="email"
          name="email"
          type="email"
          value={formData.email}
          onChange={handleChange}
          className={errors.email ? 'error' : ''}
          disabled={isSubmitting}
        />
        {errors.email && <span className="error-message">{errors.email}</span>}
      </div>

      {/* Age Field */}
      <div className="form-field">
        <label htmlFor="age">
          Age <span className="required">*</span>
        </label>
        <input
          id="age"
          name="age"
          type="number"
          value={formData.age}
          onChange={handleChange}
          className={errors.age ? 'error' : ''}
          disabled={isSubmitting}
          min="1"
          max="150"
        />
        {errors.age && <span className="error-message">{errors.age}</span>}
      </div>

      {/* Checkbox Field */}
      <div className="form-field checkbox">
        <label>
          <input
            name="agreeToTerms"
            type="checkbox"
            checked={formData.agreeToTerms}
            onChange={handleChange}
            disabled={isSubmitting}
          />
          <span>
            I agree to the terms and conditions <span className="required">*</span>
          </span>
        </label>
        {errors.agreeToTerms && (
          <span className="error-message">{errors.agreeToTerms}</span>
        )}
      </div>

      {/* Submit Error */}
      {submitError && (
        <div className="form-error">
          {submitError}
        </div>
      )}

      {/* Submit Success */}
      {submitSuccess && (
        <div className="form-success">
          Form submitted successfully!
        </div>
      )}

      {/* Submit Button */}
      <button type="submit" disabled={isSubmitting}>
        {isSubmitting ? 'Submitting...' : 'Submit'}
      </button>
    </form>
  );
};

export default FormComponent;
```

## Usage

### Basic Usage

```typescript
import { FormComponent } from './FormComponent';

function App() {
  const handleSubmit = async (data: FormData) => {
    console.log('Form data:', data);

    // API call
    const response = await fetch('/api/submit', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(data),
    });

    if (!response.ok) {
      throw new Error('Submission failed');
    }
  };

  return <FormComponent onSubmit={handleSubmit} />;
}
```

### With Initial Values

```typescript
<FormComponent
  onSubmit={handleSubmit}
  initialValues={{
    name: 'John Doe',
    email: 'john@example.com',
    age: 30,
  }}
/>
```

## OSE Platform Examples

### Zakat Calculation Form

```typescript
import React, { useState, FormEvent } from 'react';

interface ZakatFormData {
  wealth: number;
  currency: string;
  userId: string;
}

interface ZakatFormErrors {
  wealth?: string;
  currency?: string;
}

export const ZakatCalculationForm: React.FC = () => {
  const [formData, setFormData] = useState<ZakatFormData>({
    wealth: 0,
    currency: 'USD',
    userId: 'current-user-id',
  });

  const [errors, setErrors] = useState<ZakatFormErrors>({});
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [result, setResult] = useState<any>(null);

  const validate = (): ZakatFormErrors => {
    const newErrors: ZakatFormErrors = {};

    if (formData.wealth <= 0) {
      newErrors.wealth = 'Wealth must be greater than 0';
    }

    if (!formData.currency) {
      newErrors.currency = 'Currency is required';
    }

    return newErrors;
  };

  const handleSubmit = async (e: FormEvent) => {
    e.preventDefault();

    const validationErrors = validate();
    if (Object.keys(validationErrors).length > 0) {
      setErrors(validationErrors);
      return;
    }

    setIsSubmitting(true);

    try {
      const response = await fetch('/api/zakat/calculate', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(formData),
      });

      const data = await response.json();
      setResult(data);
    } catch (error) {
      alert('Calculation failed');
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <h2>Zakat Calculator</h2>

      <div className="form-field">
        <label htmlFor="wealth">Total Wealth</label>
        <input
          id="wealth"
          type="number"
          value={formData.wealth}
          onChange={(e) => setFormData(prev => ({
            ...prev,
            wealth: Number(e.target.value),
          }))}
          step="0.01"
          min="0"
        />
        {errors.wealth && <span className="error">{errors.wealth}</span>}
      </div>

      <div className="form-field">
        <label htmlFor="currency">Currency</label>
        <select
          id="currency"
          value={formData.currency}
          onChange={(e) => setFormData(prev => ({
            ...prev,
            currency: e.target.value,
          }))}
        >
          <option value="USD">USD</option>
          <option value="EUR">EUR</option>
          <option value="GBP">GBP</option>
        </select>
        {errors.currency && <span className="error">{errors.currency}</span>}
      </div>

      <button type="submit" disabled={isSubmitting}>
        {isSubmitting ? 'Calculating...' : 'Calculate Zakat'}
      </button>

      {result && (
        <div className="result">
          <h3>Results</h3>
          <p>Nisab Threshold: {result.nisabThreshold.toFixed(2)} {formData.currency}</p>
          <p>Zakat Due: {result.zakatDue ? 'Yes' : 'No'}</p>
          {result.zakatDue && (
            <p className="highlight">
              Zakat Amount: {result.zakatAmount.toFixed(2)} {formData.currency}
            </p>
          )}
        </div>
      )}
    </form>
  );
};
```

### Murabaha Application Form

```typescript
import React, { useState, FormEvent } from 'react';

interface MurabahaFormData {
  applicantName: string;
  email: string;
  phoneNumber: string;
  requestedAmount: number;
  purpose: string;
  installmentMonths: number;
}

interface MurabahaFormErrors {
  applicantName?: string;
  email?: string;
  phoneNumber?: string;
  requestedAmount?: string;
  purpose?: string;
  installmentMonths?: string;
}

export const MurabahaApplicationForm: React.FC = () => {
  const [formData, setFormData] = useState<MurabahaFormData>({
    applicantName: '',
    email: '',
    phoneNumber: '',
    requestedAmount: 0,
    purpose: '',
    installmentMonths: 12,
  });

  const [errors, setErrors] = useState<MurabahaFormErrors>({});
  const [isSubmitting, setIsSubmitting] = useState(false);

  const validate = (): MurabahaFormErrors => {
    const newErrors: MurabahaFormErrors = {};

    if (!formData.applicantName.trim()) {
      newErrors.applicantName = 'Name is required';
    }

    if (!formData.email.trim()) {
      newErrors.email = 'Email is required';
    } else if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(formData.email)) {
      newErrors.email = 'Email is invalid';
    }

    if (!formData.phoneNumber.trim()) {
      newErrors.phoneNumber = 'Phone number is required';
    }

    if (formData.requestedAmount < 1000) {
      newErrors.requestedAmount = 'Minimum amount is $1,000';
    } else if (formData.requestedAmount > 1000000) {
      newErrors.requestedAmount = 'Maximum amount is $1,000,000';
    }

    if (!formData.purpose.trim()) {
      newErrors.purpose = 'Purpose is required';
    }

    if (formData.installmentMonths < 6 || formData.installmentMonths > 60) {
      newErrors.installmentMonths = 'Installments must be between 6 and 60 months';
    }

    return newErrors;
  };

  const handleChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
    const { name, value } = e.target;
    setFormData(prev => ({
      ...prev,
      [name]: name === 'requestedAmount' || name === 'installmentMonths'
        ? Number(value)
        : value,
    }));

    // Clear error
    if (errors[name as keyof MurabahaFormErrors]) {
      setErrors(prev => ({ ...prev, [name]: undefined }));
    }
  };

  const handleSubmit = async (e: FormEvent) => {
    e.preventDefault();

    const validationErrors = validate();
    if (Object.keys(validationErrors).length > 0) {
      setErrors(validationErrors);
      return;
    }

    setIsSubmitting(true);

    try {
      const response = await fetch('/api/murabaha/applications', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(formData),
      });

      if (!response.ok) throw new Error('Application failed');

      alert('Application submitted successfully!');

      // Reset form
      setFormData({
        applicantName: '',
        email: '',
        phoneNumber: '',
        requestedAmount: 0,
        purpose: '',
        installmentMonths: 12,
      });
    } catch (error) {
      alert('Submission failed. Please try again.');
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <form onSubmit={handleSubmit} className="murabaha-form">
      <h2>Murabaha Financing Application</h2>

      <div className="form-field">
        <label htmlFor="applicantName">Full Name *</label>
        <input
          id="applicantName"
          name="applicantName"
          type="text"
          value={formData.applicantName}
          onChange={handleChange}
          disabled={isSubmitting}
        />
        {errors.applicantName && <span className="error">{errors.applicantName}</span>}
      </div>

      <div className="form-field">
        <label htmlFor="email">Email *</label>
        <input
          id="email"
          name="email"
          type="email"
          value={formData.email}
          onChange={handleChange}
          disabled={isSubmitting}
        />
        {errors.email && <span className="error">{errors.email}</span>}
      </div>

      <div className="form-field">
        <label htmlFor="phoneNumber">Phone Number *</label>
        <input
          id="phoneNumber"
          name="phoneNumber"
          type="tel"
          value={formData.phoneNumber}
          onChange={handleChange}
          disabled={isSubmitting}
        />
        {errors.phoneNumber && <span className="error">{errors.phoneNumber}</span>}
      </div>

      <div className="form-field">
        <label htmlFor="requestedAmount">Requested Amount (USD) *</label>
        <input
          id="requestedAmount"
          name="requestedAmount"
          type="number"
          value={formData.requestedAmount}
          onChange={handleChange}
          disabled={isSubmitting}
          min="1000"
          max="1000000"
          step="1000"
        />
        {errors.requestedAmount && <span className="error">{errors.requestedAmount}</span>}
      </div>

      <div className="form-field">
        <label htmlFor="installmentMonths">Installment Period (months) *</label>
        <input
          id="installmentMonths"
          name="installmentMonths"
          type="number"
          value={formData.installmentMonths}
          onChange={handleChange}
          disabled={isSubmitting}
          min="6"
          max="60"
        />
        {errors.installmentMonths && <span className="error">{errors.installmentMonths}</span>}
      </div>

      <div className="form-field">
        <label htmlFor="purpose">Purpose of Financing *</label>
        <textarea
          id="purpose"
          name="purpose"
          value={formData.purpose}
          onChange={handleChange}
          disabled={isSubmitting}
          rows={4}
        />
        {errors.purpose && <span className="error">{errors.purpose}</span>}
      </div>

      <button type="submit" disabled={isSubmitting}>
        {isSubmitting ? 'Submitting...' : 'Submit Application'}
      </button>
    </form>
  );
};
```

## Form Patterns

### Controlled Inputs

```typescript
// Text input
<input
  value={formData.field}
  onChange={(e) => setFormData(prev => ({ ...prev, field: e.target.value }))}
/>

// Checkbox
<input
  type="checkbox"
  checked={formData.field}
  onChange={(e) => setFormData(prev => ({ ...prev, field: e.target.checked }))}
/>

// Select
<select
  value={formData.field}
  onChange={(e) => setFormData(prev => ({ ...prev, field: e.target.value }))}
>
  <option value="option1">Option 1</option>
  <option value="option2">Option 2</option>
</select>
```

### Validation Patterns

```typescript
// Required field
if (!value.trim()) {
  errors.field = "Field is required";
}

// Email validation
if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)) {
  errors.email = "Email is invalid";
}

// Number range
if (value < min || value > max) {
  errors.field = `Value must be between ${min} and ${max}`;
}

// String length
if (value.length < minLength) {
  errors.field = `Minimum length is ${minLength}`;
}
```

## Best Practices

1. **Controlled Components**: Always use controlled components with state
2. **Validation**: Validate on submit, optionally on blur
3. **Error Display**: Show errors inline near the field
4. **Loading State**: Disable form during submission
5. **Success Feedback**: Show success message and reset form
6. **Accessibility**: Use proper labels, ARIA attributes, and keyboard navigation
7. **Type Safety**: Use TypeScript interfaces for form data and errors
8. **Clear Errors**: Clear field error when user starts typing
9. **Prevent Default**: Always call `e.preventDefault()` in submit handler
10. **Error Handling**: Handle both validation and submission errors

## Testing

```typescript
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { FormComponent } from './FormComponent';

describe('FormComponent', () => {
  const mockSubmit = jest.fn(() => Promise.resolve());

  beforeEach(() => {
    mockSubmit.mockClear();
  });

  it('renders all fields', () => {
    render(<FormComponent onSubmit={mockSubmit} />);

    expect(screen.getByLabelText(/name/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/age/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/terms/i)).toBeInTheDocument();
  });

  it('validates required fields', async () => {
    render(<FormComponent onSubmit={mockSubmit} />);

    fireEvent.click(screen.getByText('Submit'));

    await waitFor(() => {
      expect(screen.getByText('Name is required')).toBeInTheDocument();
      expect(screen.getByText('Email is required')).toBeInTheDocument();
    });

    expect(mockSubmit).not.toHaveBeenCalled();
  });

  it('validates email format', async () => {
    render(<FormComponent onSubmit={mockSubmit} />);

    fireEvent.change(screen.getByLabelText(/email/i), {
      target: { value: 'invalid-email' },
    });
    fireEvent.click(screen.getByText('Submit'));

    await waitFor(() => {
      expect(screen.getByText('Email is invalid')).toBeInTheDocument();
    });
  });

  it('submits valid form', async () => {
    render(<FormComponent onSubmit={mockSubmit} />);

    fireEvent.change(screen.getByLabelText(/name/i), {
      target: { value: 'John Doe' },
    });
    fireEvent.change(screen.getByLabelText(/email/i), {
      target: { value: 'john@example.com' },
    });
    fireEvent.change(screen.getByLabelText(/age/i), {
      target: { value: '30' },
    });
    fireEvent.click(screen.getByLabelText(/terms/i));
    fireEvent.click(screen.getByText('Submit'));

    await waitFor(() => {
      expect(mockSubmit).toHaveBeenCalledWith({
        name: 'John Doe',
        email: 'john@example.com',
        age: 30,
        agreeToTerms: true,
      });
    });
  });

  it('handles submission error', async () => {
    const mockErrorSubmit = jest.fn(() => Promise.reject(new Error('API Error')));

    render(<FormComponent onSubmit={mockErrorSubmit} />);

    // Fill and submit form
    fireEvent.change(screen.getByLabelText(/name/i), {
      target: { value: 'John Doe' },
    });
    fireEvent.change(screen.getByLabelText(/email/i), {
      target: { value: 'john@example.com' },
    });
    fireEvent.change(screen.getByLabelText(/age/i), {
      target: { value: '30' },
    });
    fireEvent.click(screen.getByLabelText(/terms/i));
    fireEvent.click(screen.getByText('Submit'));

    await waitFor(() => {
      expect(screen.getByText('API Error')).toBeInTheDocument();
    });
  });
});
```
