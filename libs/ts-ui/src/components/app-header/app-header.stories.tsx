import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { AppHeader } from "./app-header";
import { Button } from "../button/button";

const meta = {
  title: "Navigation/AppHeader",
  component: AppHeader,
  parameters: { layout: "padded" },
} satisfies Meta<typeof AppHeader>;

export default meta;
type Story = StoryObj<typeof meta>;

export const WithBack: Story = { args: { title: "Workout Details", onBack: () => {} } };
export const WithoutBack: Story = { args: { title: "Home" } };
export const WithSubtitle: Story = {
  args: { title: "Workouts", subtitle: "Today · 3 sessions", onBack: () => {} },
};
export const WithTrailing: Story = {
  args: {
    title: "Profile",
    onBack: () => {},
    trailing: (
      <Button variant="ghost" size="sm">
        Edit
      </Button>
    ),
  },
};
