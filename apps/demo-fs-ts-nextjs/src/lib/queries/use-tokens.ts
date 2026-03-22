import { useQuery } from "@tanstack/react-query";
import * as tokensApi from "../api/tokens";

export function useTokenClaims() {
  return useQuery({
    queryKey: ["tokenClaims"],
    queryFn: () => tokensApi.getTokenClaims(),
    retry: false,
  });
}

export function useJwks() {
  return useQuery({
    queryKey: ["jwks"],
    queryFn: () => tokensApi.getJwks(),
  });
}
