import { Suspense } from "react";
import { CvContent } from "@/contexts/cv/presentation/CvContent";

export default function CV() {
  return (
    <Suspense fallback={<div>Loading...</div>}>
      <CvContent />
    </Suspense>
  );
}
