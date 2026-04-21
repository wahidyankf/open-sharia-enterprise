import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { TabBar, type TabItem } from "./tab-bar";

const tabs: TabItem[] = [
  { id: "home", label: "Home", icon: "home" },
  { id: "history", label: "History", icon: "history" },
  { id: "calendar", label: "Calendar", icon: "calendar" },
  { id: "settings", label: "Settings", icon: "settings" },
];

const meta = {
  title: "Navigation/TabBar",
  component: TabBar,
  parameters: { layout: "fullscreen" },
} satisfies Meta<typeof TabBar>;

export default meta;
type Story = StoryObj<typeof meta>;

export const HomeActive: Story = { args: { tabs, current: "home", onChange: () => {} } };
export const HistoryActive: Story = { args: { tabs, current: "history", onChange: () => {} } };
