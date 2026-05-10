import { Suspense } from "react";
import { HomeContent } from "@/contexts/home/presentation/HomeContent";

export default function Home() {
  return (
    <Suspense fallback={<div>Loading...</div>}>
      <HomeContent />
    </Suspense>
  );
}
