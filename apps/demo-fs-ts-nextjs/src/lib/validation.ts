const PASSWORD_MIN_LENGTH = 12;
const PASSWORD_UPPERCASE_RE = /[A-Z]/;
const PASSWORD_SPECIAL_RE = /[^A-Za-z0-9]/;
const EMAIL_RE = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
const USERNAME_RE = /^[a-zA-Z0-9_-]{3,50}$/;

export function validatePassword(password: string): string | null {
  if (password.length < PASSWORD_MIN_LENGTH) {
    return `Password must be at least ${PASSWORD_MIN_LENGTH} characters long`;
  }
  if (!PASSWORD_UPPERCASE_RE.test(password)) {
    return "Password must contain at least one uppercase letter";
  }
  if (!PASSWORD_SPECIAL_RE.test(password)) {
    return "Password must contain at least one special character";
  }
  return null;
}

export function validateEmail(email: string): string | null {
  if (!EMAIL_RE.test(email)) return "Invalid email format";
  return null;
}

export function validateUsername(username: string): string | null {
  if (!USERNAME_RE.test(username)) {
    return "Username must be 3-50 characters and contain only letters, digits, underscores, and hyphens";
  }
  return null;
}
