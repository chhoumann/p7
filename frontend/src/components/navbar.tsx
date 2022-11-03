import React from "react";


function Navbar() {
    return (
        <nav>
            <div className="container flex items-center p-4">
                <span className="ml-10 text-stone-300 font-bold link link-underline link-underline-black text-xl hover:text-sky-400 cursor-pointer">Home</span>
                <span className="absolute text-stone-300 font-bold link link-underline link-underline-black text-xl right-20 hover:text-sky-400 cursor-pointer">
                    Login
                </span>
            </div>
        </nav>
    );
}

export default Navbar;
