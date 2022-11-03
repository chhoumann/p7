import { NextPage } from 'next';
import { useRouter } from 'next/router'
import { trpc } from "../../utils/trpc"
import React, { useState } from 'react'
import Link from 'next/link';

const create: NextPage = () => {


    return (
        <div className='container flex justify-center items-center w-full h-[75vh]'>
            <div className='flex flex-col mt-40 w-[60vh] h-full'>
                <form className="mb-6">
                    <label className="flex flex-col mb-2 font-bold text-lg text-grey-darkest">
                        Enter Title
                        <input type="text" name="" className='border py-2 px-3 text-grey-darkest'/>
                    </label>
                </form>
                <form className="mb-6">
                    <label className="flex flex-col mb-2 font-bold text-lg text-grey-darkest">
                        Create test
                        <textarea name="" className='border-solid border-2 border-gray-500'/>
                    </label>
                </form>
                <form className="mb-6">
                    <label className="flex flex-col mb-2 font-bold text-lg text-grey-darkest">
                        Function template
                        <textarea name="" className='border-solid border-2 border-gray-500'/>
                    </label>
                </form>
                <form className="mb-6">
                    <label className="flex flex-col mb-2 font-bold text-lg text-grey-darkest">
                        Description
                        <textarea name="" className='border-solid border-2 border-gray-500'/>
                    </label>
                </form>
                <div className='mb-auto' />
                <div className='flex flex-row gap-4 mx-3 my-3 pt-3 pb-3 sticky bottom-0 bg-white'>
                    <button className='bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black'>
                        Create exercise
                    </button>
                </div>
            </div>
        </div>
    )
}

export default create;