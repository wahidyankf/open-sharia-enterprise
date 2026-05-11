import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Input } from "../input/input";
import { Label } from "./label";

const meta: Meta<typeof Label> = {
  title: "Forms/Label",
  component: Label,
  tags: ["autodocs"],
  argTypes: {
    children: { control: "text" },
    htmlFor: { control: "text" },
  },
  args: {
    children: "Label text",
  },
};

export default meta;

type Story = StoryObj<typeof Label>;

export const Default: Story = {};

export const Interactive: Story = {
  args: {
    children: "Your label",
    htmlFor: "interactive-input",
  },
};

export const PairedWithInput: Story = {
  name: "Paired With Input",
  render: () => (
    <div className="grid w-80 gap-1.5">
      <Label htmlFor="email">Email address</Label>
      <Input id="email" type="email" placeholder="you@example.com" />
    </div>
  ),
};

export const PairedWithDisabledInput: Story = {
  name: "Paired With Disabled Input (peer-disabled)",
  render: () => (
    <div className="grid w-80 gap-1.5">
      <Label htmlFor="disabled-input">Disabled field</Label>
      <Input id="disabled-input" disabled placeholder="Cannot edit this field" />
    </div>
  ),
};

export const Required: Story = {
  name: "Required Field",
  render: () => (
    <div className="grid w-80 gap-1.5">
      <Label htmlFor="required-input">
        Full name <span className="text-destructive">*</span>
      </Label>
      <Input id="required-input" placeholder="John Doe" required />
    </div>
  ),
};

export const InvalidField: Story = {
  name: "Invalid Field",
  render: () => (
    <div className="grid w-80 gap-1.5">
      <Label htmlFor="invalid-input">Email</Label>
      <Input id="invalid-input" type="email" aria-invalid={true} defaultValue="not-an-email" />
      <p className="text-xs text-destructive">Please enter a valid email address.</p>
    </div>
  ),
};

export const FormGroup: Story = {
  name: "Form Group",
  render: () => (
    <div className="grid w-80 gap-4">
      <div className="grid gap-1.5">
        <Label htmlFor="form-name">Name</Label>
        <Input id="form-name" placeholder="John Doe" />
      </div>
      <div className="grid gap-1.5">
        <Label htmlFor="form-email">Email</Label>
        <Input id="form-email" type="email" placeholder="you@example.com" />
      </div>
      <div className="grid gap-1.5">
        <Label htmlFor="form-password">Password</Label>
        <Input id="form-password" type="password" placeholder="••••••••" />
      </div>
    </div>
  ),
};
