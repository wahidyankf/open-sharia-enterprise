import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { SideNav } from "./side-nav";
import type { TabItem } from "../tab-bar/tab-bar";

const tabs: TabItem[] = [
  { id: "home", label: "Home", icon: "home" },
  { id: "history", label: "History", icon: "history" },
  { id: "calendar", label: "Calendar", icon: "calendar" },
  { id: "settings", label: "Settings", icon: "settings" },
];

const brand = { name: "OrganicLever", icon: "dumbbell", hue: "teal" as const };

const meta = {
  title: "Navigation/SideNav",
  component: SideNav,
  parameters: { layout: "fullscreen" },
} satisfies Meta<typeof SideNav>;

export default meta;
type Story = StoryObj<typeof meta>;

export const HomeActive: Story = {
  args: { brand, tabs, current: "home", onChange: () => {} },
};

export const HistoryActive: Story = {
  args: { brand, tabs, current: "history", onChange: () => {} },
};
