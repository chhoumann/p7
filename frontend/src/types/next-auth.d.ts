import { DefaultSession, User as OldUser } from "next-auth";
import { User as PrismaUser } from "@prisma/client";

// Don't want to conflict with generated Role (@prisma/client)
type RoleValue = 'student' | 'teacher';

declare module "next-auth" {
  /**
   * Returned by `useSession`, `getSession` and received as a prop on the `SessionProvider` React Context
   */
  interface Session {
    user?: {
      id: string;
      role: RoleValue;
    } & DefaultSession["user"];
  }

  interface User extends OldUser {
    roleName: string;
  }
}

declare module "@prisma/client" {
  interface User extends PrismaUser {
    roleName: RoleValue;
  }
}