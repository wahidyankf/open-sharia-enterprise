import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Button } from "../button/button";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from "./dialog";

const meta: Meta<typeof Dialog> = {
  title: "Overlay/Dialog",
  component: Dialog,
  tags: ["autodocs"],
  parameters: {
    layout: "centered",
  },
};

export default meta;

type Story = StoryObj<typeof Dialog>;

export const Default: Story = {
  render: () => (
    <Dialog>
      <DialogTrigger asChild>
        <Button variant="outline">Open dialog</Button>
      </DialogTrigger>
      <DialogContent>
        <DialogHeader>
          <DialogTitle>Dialog title</DialogTitle>
          <DialogDescription>This is a dialog description providing more context.</DialogDescription>
        </DialogHeader>
        <p className="text-sm">Dialog body content goes here.</p>
        <DialogFooter>
          <Button variant="outline">Cancel</Button>
          <Button>Confirm</Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  ),
};

export const WithConfirmAction: Story = {
  name: "With Confirm Action",
  render: () => (
    <Dialog>
      <DialogTrigger asChild>
        <Button>Open</Button>
      </DialogTrigger>
      <DialogContent>
        <DialogHeader>
          <DialogTitle>Confirm action</DialogTitle>
          <DialogDescription>Are you sure you want to proceed? This action cannot be undone.</DialogDescription>
        </DialogHeader>
        <DialogFooter>
          <Button variant="outline">Cancel</Button>
          <Button variant="destructive">Delete</Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  ),
};

export const WithoutCloseButton: Story = {
  name: "Without Close Button",
  render: () => (
    <Dialog>
      <DialogTrigger asChild>
        <Button variant="outline">Open (no close button)</Button>
      </DialogTrigger>
      <DialogContent showCloseButton={false}>
        <DialogHeader>
          <DialogTitle>No close button</DialogTitle>
          <DialogDescription>This dialog hides the default close button in the top-right corner.</DialogDescription>
        </DialogHeader>
        <DialogFooter>
          <Button variant="outline">Cancel</Button>
          <Button>Done</Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  ),
};

export const FormDialog: Story = {
  name: "Form Dialog",
  render: () => (
    <Dialog>
      <DialogTrigger asChild>
        <Button>Edit profile</Button>
      </DialogTrigger>
      <DialogContent>
        <DialogHeader>
          <DialogTitle>Edit profile</DialogTitle>
          <DialogDescription>Make changes to your profile. Click save when you are done.</DialogDescription>
        </DialogHeader>
        <div className="grid gap-4 py-2">
          <div className="grid gap-1.5">
            <label htmlFor="name" className="text-sm font-medium">
              Name
            </label>
            <input
              id="name"
              defaultValue="John Doe"
              className="h-9 rounded-md border border-input bg-transparent px-3 py-1 text-sm outline-none"
            />
          </div>
          <div className="grid gap-1.5">
            <label htmlFor="email" className="text-sm font-medium">
              Email
            </label>
            <input
              id="email"
              defaultValue="john@example.com"
              className="h-9 rounded-md border border-input bg-transparent px-3 py-1 text-sm outline-none"
            />
          </div>
        </div>
        <DialogFooter>
          <Button variant="outline">Cancel</Button>
          <Button>Save changes</Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  ),
};

export const LongContent: Story = {
  name: "Long Content",
  render: () => (
    <Dialog>
      <DialogTrigger asChild>
        <Button variant="outline">Open long dialog</Button>
      </DialogTrigger>
      <DialogContent>
        <DialogHeader>
          <DialogTitle>Terms of service</DialogTitle>
          <DialogDescription>Please read the terms carefully before proceeding.</DialogDescription>
        </DialogHeader>
        <div className="max-h-60 space-y-2 overflow-y-auto text-sm text-muted-foreground">
          <p>
            Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et
            dolore magna aliqua.
          </p>
          <p>
            Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
          </p>
          <p>Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.</p>
          <p>
            Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est
            laborum.
          </p>
          <p>
            Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem
            aperiam.
          </p>
        </div>
        <DialogFooter>
          <Button variant="outline">Decline</Button>
          <Button>Accept</Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  ),
};
