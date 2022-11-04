import { signIn, useSession } from "next-auth/react";
import React from "react";

function Navbar() {
    const { data: session } = useSession();

    return (
        <nav>
            <div className="border-b-2 p-4 flex justify-between align-middle">
                <span />
                <div>
                    {session ? (
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
        <div className="flex flex-col">
            <span className="mr-2 p-1 px-3 rounded-full text-green-300 bg-sky-700 outline outline-gray-800 outline-1 font-bold text-lg">
                {name.at(0)}
            </span>
        </div>
    );
}

function LogInBtn() {
    return (
        <button
            className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
            onClick={() => signIn("google")}
        >
            Sign in
        </button>
    );
}

export default Navbar;
