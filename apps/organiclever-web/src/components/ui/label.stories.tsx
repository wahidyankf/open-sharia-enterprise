import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Label } from "./label";
import { Input } from "./input";

const meta: Meta<typeof Label> = {
  title: "UI/Label",
  component: Label,
  tags: ["autodocs"],
};

export default meta;
type Story = StoryObj<typeof Label>;

export const Default: Story = {
  args: {
    children: "Email address",
    htmlFor: "email",
  },
};

export const WithInput: Story = {
  render: () => (
    <div className="grid w-full max-w-sm items-center gap-1.5">
      <Label htmlFor="email-input">Email</Label>
      <Input type="email" id="email-input" placeholder="you@example.com" />
    </div>
  ),
};

export const WithPasswordInput: Story = {
  render: () => (
    <div className="grid w-full max-w-sm items-center gap-1.5">
      <Label htmlFor="password-input">Password</Label>
      <Input type="password" id="password-input" placeholder="Enter your password" />
    </div>
  ),
};

export const WithDisabledInput: Story = {
  render: () => (
    <div className="grid w-full max-w-sm items-center gap-1.5">
      <Label htmlFor="disabled-input">Username (read-only)</Label>
      <Input type="text" id="disabled-input" disabled defaultValue="alice_johnson" />
    </div>
  ),
};

export const FormGroup: Story = {
  render: () => (
    <div className="grid w-full max-w-sm gap-4">
      <div className="grid items-center gap-1.5">
        <Label htmlFor="first-name">First name</Label>
        <Input type="text" id="first-name" placeholder="Alice" />
      </div>
      <div className="grid items-center gap-1.5">
        <Label htmlFor="last-name">Last name</Label>
        <Input type="text" id="last-name" placeholder="Johnson" />
      </div>
      <div className="grid items-center gap-1.5">
        <Label htmlFor="email-form">Email</Label>
        <Input type="email" id="email-form" placeholder="alice@example.com" />
      </div>
    </div>
  ),
};
