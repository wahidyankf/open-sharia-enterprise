import { greet } from "@open-sharia-enterprise/ts-demo-libs";

export default function Home() {
  const message = greet("Next.js");

  return (
    <div className="min-h-screen flex items-center justify-center">
      <main className="text-center">
        <h1 className="text-4xl font-bold mb-4">Welcome to Demo Next.js App</h1>
        <p className="text-xl text-gray-600">
          This is a demo app in the Nx monorepo
        </p>
        <p className="text-2xl font-semibold mt-6 text-blue-600">{message}</p>
        <p className="text-sm text-gray-500 mt-2">
          (Message imported from @open-sharia-enterprise/ts-demo-libs)
        </p>
      </main>
    </div>
  );
}
