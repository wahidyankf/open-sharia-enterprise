import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { ProgressRing } from "./progress-ring";

const meta = {
  title: "Foundation/ProgressRing",
  component: ProgressRing,
  parameters: { layout: "centered" },
} satisfies Meta<typeof ProgressRing>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Full: Story = { args: { progress: 1 } };
export const Half: Story = { args: { progress: 0.5 } };
export const Empty: Story = { args: { progress: 0 } };
export const CustomColor: Story = { args: { progress: 0.7, color: "var(--hue-terracotta)" } };
