import React from "react";


function Navbar() {
    return (
        <nav>
            <div className="container flex items-center p-4">
                <span className="ml-10 text-yellow-400 font-bold link link-underline link-underline-black text-xl">Home</span>
                <span className="absolute text-yellow-400 font-bold link link-underline link-underline-black text-xl right-20">
                    Login
                </span>
            </div>
        </nav>
    );
}

export default Navbar;
