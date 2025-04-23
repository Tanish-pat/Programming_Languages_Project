"use client"

import { useEffect, useState } from "react"
import { Bar, BarChart, CartesianGrid, Legend, ResponsiveContainer, Tooltip, XAxis, YAxis } from "recharts"

const data = [
  {
    name: "Electronics",
    inStock: 120,
    lowStock: 5,
    outOfStock: 2,
  },
  {
    name: "Clothing",
    inStock: 80,
    lowStock: 10,
    outOfStock: 0,
  },
  {
    name: "Home",
    inStock: 45,
    lowStock: 8,
    outOfStock: 3,
  },
  {
    name: "Beauty",
    inStock: 60,
    lowStock: 3,
    outOfStock: 1,
  },
  {
    name: "Sports",
    inStock: 35,
    lowStock: 6,
    outOfStock: 4,
  },
  {
    name: "Books",
    inStock: 90,
    lowStock: 2,
    outOfStock: 0,
  },
]

export function InventoryChart() {
  const [isMounted, setIsMounted] = useState(false)

  useEffect(() => {
    setIsMounted(true)
  }, [])

  if (!isMounted) {
    return <div className="h-[300px] flex items-center justify-center">Loading chart...</div>
  }

  return (
    <ResponsiveContainer width="100%" height={350}>
      <BarChart data={data}>
        <CartesianGrid strokeDasharray="3 3" />
        <XAxis dataKey="name" />
        <YAxis />
        <Tooltip />
        <Legend />
        <Bar dataKey="inStock" name="In Stock" fill="#22c55e" />
        <Bar dataKey="lowStock" name="Low Stock" fill="#eab308" />
        <Bar dataKey="outOfStock" name="Out of Stock" fill="#ef4444" />
      </BarChart>
    </ResponsiveContainer>
  )
}
