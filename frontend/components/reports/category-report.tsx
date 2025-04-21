"use client"

import { Bar, BarChart, CartesianGrid, Legend, ResponsiveContainer, Tooltip, XAxis, YAxis } from "recharts"

const data = [
  {
    name: "Electronics",
    sales: 12000,
    items: 45,
  },
  {
    name: "Furniture",
    sales: 8000,
    items: 32,
  },
  {
    name: "Office Supplies",
    sales: 5000,
    items: 78,
  },
  {
    name: "Books",
    sales: 2000,
    items: 12,
  },
  {
    name: "Food & Beverages",
    sales: 3000,
    items: 23,
  },
]

export function CategoryReport() {
  return (
    <ResponsiveContainer width="100%" height={400}>
      <BarChart data={data}>
        <CartesianGrid strokeDasharray="3 3" />
        <XAxis dataKey="name" />
        <YAxis yAxisId="left" orientation="left" stroke="#3b82f6" />
        <YAxis yAxisId="right" orientation="right" stroke="#10b981" />
        <Tooltip />
        <Legend />
        <Bar yAxisId="left" dataKey="sales" name="Sales ($)" fill="#3b82f6" />
        <Bar yAxisId="right" dataKey="items" name="Items" fill="#10b981" />
      </BarChart>
    </ResponsiveContainer>
  )
}
