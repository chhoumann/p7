import { signIn, signOut, useSession } from "next-auth/react";
import Link from "next/link";
import React from "react";
import { LogOut, User } from "react-feather";
import { Menu } from "@headlessui/react";
import clsx from "clsx";

function Navbar() {
    const { status, data: session } = useSession();

    return (
        <nav>
            <div className="flex items-center p-4 flex-row align-middle justify-between">
                <div className="flex flex-row align-middle gap-4">
                    <Link href="/">
                        <a className="ml-10 font-bold link link-underline link-underline-black text-xl hover:text-sky-400 cursor-pointer">
                            Home
                        </a>
                    </Link>

                    {session ? (
                        <Link href="/syllabi">
                            <a className="ml-10 font-bold link link-underline link-underline-black text-xl hover:text-sky-400 cursor-pointer">
                                Courses
                            </a>
                        </Link>
                    ) : null}
                </div>
                <div>
                    {status !== "loading" && session ? (
                        <UserProfile name={session.user?.name ?? ""} />
                    ) : (
                        <LogInBtn />
                    )}
                </div>
            </div>
        </nav>
    );
}

function UserProfile({ name }: { name: string }) {
    return (
        <Menu>
            <Menu.Button>
                <User />
            </Menu.Button>
            <Menu.Items className="absolute border p-4 right-4 z-10 pointer-events-auto rounded-lg bg-white w-48">
                <Menu.Item>
                    <div className="container">
                        <p className="text-gray-700 font-semibold text-center">{name}</p>
                    </div>
                </Menu.Item>
                <Menu.Item>
                    <div className="border-b my-4" />
                </Menu.Item>
                <Menu.Item>
                    {({ active }) => (
                        <button
                            className={clsx(
                                active && "bg-gray-100",
                                "p-2 w-full rounded font-semibold",
                                "flex flex-row gap-4 items-center justify-center"
                            )}
                            onClick={() => signOut()}
                        >
                            <LogOut /> Log Out
                        </button>
                    )}
                </Menu.Item>
            </Menu.Items>
        </Menu>
    );
}

function LogInBtn() {
    return (
        <button
            className="absolute font-bold link link-underline link-underline-black text-xl right-20 hover:text-sky-400 cursor-pointer"
            onClick={() => signIn("google")}
        >
            Sign in
        </button>
    );
}

export default Navbar;
