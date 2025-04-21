"use client"

import { useEffect, useState } from "react"
import { BoxIcon, DollarSign, Package, ShoppingCart, Truck } from "lucide-react"

import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card"
import { fetchStats } from "@/lib/api"
import { formatCurrency } from "@/lib/utils"

export function DashboardStats() {
  const [stats, setStats] = useState({
    totalItems: 0,
    totalCategories: 0,
    totalSuppliers: 0,
    lowStockItems: 0,
    totalValue: 0,
  })
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    const getStats = async () => {
      try {
        // Fetch dashboard statistics from the API
        const data = await fetchStats()
        setStats(data)
      } catch (error) {
        console.error("Failed to fetch stats:", error)
      } finally {
        setLoading(false)
      }
    }

    getStats()
  }, [])

  return (
    <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-5">
      <Card>
        <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
          <CardTitle className="text-sm font-medium">Total Inventory</CardTitle>
          <Package className="h-4 w-4 text-muted-foreground" />
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{loading ? "..." : stats.totalItems}</div>
          <p className="text-xs text-muted-foreground">Items in stock</p>
        </CardContent>
      </Card>
      <Card>
        <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
          <CardTitle className="text-sm font-medium">Categories</CardTitle>
          <BoxIcon className="h-4 w-4 text-muted-foreground" />
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{loading ? "..." : stats.totalCategories}</div>
          <p className="text-xs text-muted-foreground">Product categories</p>
        </CardContent>
      </Card>
      <Card>
        <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
          <CardTitle className="text-sm font-medium">Suppliers</CardTitle>
          <Truck className="h-4 w-4 text-muted-foreground" />
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{loading ? "..." : stats.totalSuppliers}</div>
          <p className="text-xs text-muted-foreground">Active suppliers</p>
        </CardContent>
      </Card>
      <Card>
        <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
          <CardTitle className="text-sm font-medium">Low Stock</CardTitle>
          <ShoppingCart className="h-4 w-4 text-muted-foreground" />
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{loading ? "..." : stats.lowStockItems}</div>
          <p className="text-xs text-muted-foreground">Items need restocking</p>
        </CardContent>
      </Card>
      <Card>
        <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
          <CardTitle className="text-sm font-medium">Inventory Value</CardTitle>
          <DollarSign className="h-4 w-4 text-muted-foreground" />
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{loading ? "..." : formatCurrency(stats.totalValue)}</div>
          <p className="text-xs text-muted-foreground">Total value of inventory</p>
        </CardContent>
      </Card>
    </div>
  )
}
