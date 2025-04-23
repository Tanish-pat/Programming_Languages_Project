"use client"

import { useState, useEffect } from "react"
import { Card, CardContent, CardHeader, CardTitle, CardFooter } from "@/components/ui/card"
import {
  Users,
  Package,
  Star,
  BarChart2,
  Tag,
  Layers,
  TrendingUp,
  TrendingDown,
  ShoppingCart,
  AlertCircle,
} from "lucide-react"
import { CustomerAPI, ProductAPI, ReviewAPI, InventoryAPI } from "@/lib/api"
import { useToast } from "@/components/ui/use-toast"
import { Button } from "@/components/ui/button"
import { Progress } from "@/components/ui/progress"
import { useRouter } from "next/navigation"

interface DashboardStats {
  customers: number
  products: number
  reviews: number
  inventory: {
    total: number
    lowStock: number
  }
}

export default function Dashboard() {
  const router = useRouter()
  const { toast } = useToast()
  const [stats, setStats] = useState<DashboardStats>({
    customers: 0,
    products: 0,
    reviews: 0,
    inventory: {
      total: 0,
      lowStock: 0,
    },
  })
  const [isLoading, setIsLoading] = useState(true)

  useEffect(() => {
    const fetchStats = async () => {
      setIsLoading(true)
      try {
        // Fetch all data in parallel
        const [customers, products, reviews, inventory] = await Promise.all([
          CustomerAPI.getAll(),
          ProductAPI.getAll(),
          ReviewAPI.getAll(),
          InventoryAPI.getAll(),
        ])

        // Calculate low stock items (less than 10 units)
        const lowStockItems = inventory.filter((item) => item.quantity < 10)

        setStats({
          customers: customers.length,
          products: products.length,
          reviews: reviews.length,
          inventory: {
            total: inventory.length,
            lowStock: lowStockItems.length,
          },
        })
      } catch (error) {
        toast({
          title: "Error",
          description: "Failed to fetch dashboard statistics",
          variant: "destructive",
        })
      } finally {
        setIsLoading(false)
      }
    }

    fetchStats()
  }, [])

  // Calculate inventory health percentage
  const inventoryHealthPercentage =
    stats.inventory.total > 0
      ? Math.max(0, Math.min(100, 100 - (stats.inventory.lowStock / stats.inventory.total) * 100))
      : 100

  return (
    <div className="space-y-6">
      <div className="flex flex-col md:flex-row justify-between items-start md:items-center gap-4">
        <h1 className="text-2xl font-bold tracking-tight">Dashboard Overview</h1>
        <div className="flex items-center gap-2">
          <Button variant="outline" onClick={() => router.push("/reports")}>
            Generate Reports
          </Button>
          <Button onClick={() => router.push("/inventory")}>
            <AlertCircle className="mr-2 h-4 w-4" /> Manage Low Stock
          </Button>
        </div>
      </div>

      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-4">
        <Card className="border-l-4 border-l-blue-500">
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Total Customers</CardTitle>
            <Users className="h-4 w-4 text-blue-500" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{isLoading ? "..." : stats.customers}</div>
            <p className="text-xs text-gray-500">
              <TrendingUp className="mr-1 h-3 w-3 inline text-green-500" />
              +5% from last month
            </p>
          </CardContent>
          <CardFooter className="pt-0 pb-2">
            <Button variant="ghost" size="sm" className="text-xs px-0" onClick={() => router.push("/customers")}>
              View all customers →
            </Button>
          </CardFooter>
        </Card>

        <Card className="border-l-4 border-l-purple-500">
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Total Products</CardTitle>
            <Package className="h-4 w-4 text-purple-500" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{isLoading ? "..." : stats.products}</div>
            <p className="text-xs text-gray-500">
              <TrendingUp className="mr-1 h-3 w-3 inline text-green-500" />
              +12% from last month
            </p>
          </CardContent>
          <CardFooter className="pt-0 pb-2">
            <Button variant="ghost" size="sm" className="text-xs px-0" onClick={() => router.push("/products")}>
              Manage products →
            </Button>
          </CardFooter>
        </Card>

        <Card className="border-l-4 border-l-yellow-500">
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Total Reviews</CardTitle>
            <Star className="h-4 w-4 text-yellow-500" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{isLoading ? "..." : stats.reviews}</div>
            <p className="text-xs text-gray-500">
              <TrendingUp className="mr-1 h-3 w-3 inline text-green-500" />
              +8% from last month
            </p>
          </CardContent>
          <CardFooter className="pt-0 pb-2">
            <Button variant="ghost" size="sm" className="text-xs px-0" onClick={() => router.push("/reviews")}>
              View all reviews →
            </Button>
          </CardFooter>
        </Card>

        <Card className="border-l-4 border-l-red-500">
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Low Stock Items</CardTitle>
            <AlertCircle className="h-4 w-4 text-red-500" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{isLoading ? "..." : stats.inventory.lowStock}</div>
            <p className="text-xs text-gray-500">
              <TrendingDown className="mr-1 h-3 w-3 inline text-red-500" />
              +2 since yesterday
            </p>
          </CardContent>
          <CardFooter className="pt-0 pb-2">
            <Button variant="ghost" size="sm" className="text-xs px-0" onClick={() => router.push("/inventory")}>
              Update inventory →
            </Button>
          </CardFooter>
        </Card>
      </div>

      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
        <Card className="col-span-2">
          <CardHeader>
            <CardTitle>Inventory Health</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-6">
              <div className="space-y-2">
                <div className="flex justify-between items-center">
                  <span className="text-sm font-medium">Overall Inventory Status</span>
                  <span className="text-sm font-medium">{inventoryHealthPercentage.toFixed(0)}%</span>
                </div>
                <Progress value={inventoryHealthPercentage} className="h-2" />
              </div>

              <div className="grid grid-cols-2 gap-4">
                <div className="space-y-2">
                  <div className="flex justify-between items-center">
                    <span className="text-sm">Total Products</span>
                    <span className="text-sm font-medium">{stats.products}</span>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm">In Stock</span>
                    <span className="text-sm font-medium">{stats.inventory.total - stats.inventory.lowStock}</span>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm">Low Stock</span>
                    <span className="text-sm font-medium text-yellow-600">{stats.inventory.lowStock}</span>
                  </div>
                </div>

                <div className="space-y-2">
                  <div className="flex justify-between items-center">
                    <span className="text-sm">Active Customers</span>
                    <span className="text-sm font-medium">{stats.customers}</span>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm">Total Reviews</span>
                    <span className="text-sm font-medium">{stats.reviews}</span>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm">Avg. Rating</span>
                    <span className="text-sm font-medium">4.2/5</span>
                  </div>
                </div>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>Quick Actions</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <Button variant="outline" className="w-full justify-start" onClick={() => router.push("/products/new")}>
                <Package className="mr-2 h-4 w-4" /> Add New Product
              </Button>
              <Button variant="outline" className="w-full justify-start" onClick={() => router.push("/customers/new")}>
                <Users className="mr-2 h-4 w-4" /> Add New Customer
              </Button>
              <Button variant="outline" className="w-full justify-start" onClick={() => router.push("/inventory")}>
                <BarChart2 className="mr-2 h-4 w-4" /> Update Inventory
              </Button>
              <Button variant="outline" className="w-full justify-start" onClick={() => router.push("/coupons/new")}>
                <Tag className="mr-2 h-4 w-4" /> Create Coupon
              </Button>
              <Button variant="outline" className="w-full justify-start" onClick={() => router.push("/categories/new")}>
                <Layers className="mr-2 h-4 w-4" /> Add Category
              </Button>
            </div>
          </CardContent>
        </Card>
      </div>

      <div className="grid gap-4 md:grid-cols-2">
        <Card>
          <CardHeader>
            <CardTitle>Recent Activity</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div className="flex items-start gap-4">
                <div className="bg-blue-100 p-2 rounded-full">
                  <Package className="h-4 w-4 text-blue-500" />
                </div>
                <div>
                  <p className="text-sm font-medium">New product added</p>
                  <p className="text-xs text-gray-500">Wireless Headphones</p>
                  <p className="text-xs text-gray-400">2 minutes ago</p>
                </div>
              </div>
              <div className="flex items-start gap-4">
                <div className="bg-green-100 p-2 rounded-full">
                  <Users className="h-4 w-4 text-green-500" />
                </div>
                <div>
                  <p className="text-sm font-medium">New customer registered</p>
                  <p className="text-xs text-gray-500">John Smith</p>
                  <p className="text-xs text-gray-400">15 minutes ago</p>
                </div>
              </div>
              <div className="flex items-start gap-4">
                <div className="bg-purple-100 p-2 rounded-full">
                  <ShoppingCart className="h-4 w-4 text-purple-500" />
                </div>
                <div>
                  <p className="text-sm font-medium">Inventory updated</p>
                  <p className="text-xs text-gray-500">Smartphone Case: +25 units</p>
                  <p className="text-xs text-gray-400">1 hour ago</p>
                </div>
              </div>
              <div className="flex items-start gap-4">
                <div className="bg-yellow-100 p-2 rounded-full">
                  <Star className="h-4 w-4 text-yellow-500" />
                </div>
                <div>
                  <p className="text-sm font-medium">New review received</p>
                  <p className="text-xs text-gray-500">5-star review for Bluetooth Speaker</p>
                  <p className="text-xs text-gray-400">3 hours ago</p>
                </div>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>System Status</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div className="flex justify-between items-center">
                <div className="flex items-center gap-2">
                  <div className="h-3 w-3 rounded-full bg-green-500"></div>
                  <span className="text-sm font-medium">Database</span>
                </div>
                <span className="text-xs px-2 py-1 bg-green-100 text-green-800 rounded-full">Operational</span>
              </div>
              <div className="flex justify-between items-center">
                <div className="flex items-center gap-2">
                  <div className="h-3 w-3 rounded-full bg-green-500"></div>
                  <span className="text-sm font-medium">API Services</span>
                </div>
                <span className="text-xs px-2 py-1 bg-green-100 text-green-800 rounded-full">Operational</span>
              </div>
              <div className="flex justify-between items-center">
                <div className="flex items-center gap-2">
                  <div className="h-3 w-3 rounded-full bg-green-500"></div>
                  <span className="text-sm font-medium">Storage</span>
                </div>
                <span className="text-xs px-2 py-1 bg-green-100 text-green-800 rounded-full">Operational</span>
              </div>
              <div className="flex justify-between items-center">
                <div className="flex items-center gap-2">
                  <div className="h-3 w-3 rounded-full bg-green-500"></div>
                  <span className="text-sm font-medium">Authentication</span>
                </div>
                <span className="text-xs px-2 py-1 bg-green-100 text-green-800 rounded-full">Operational</span>
              </div>
              <div className="pt-2 border-t">
                <div className="flex justify-between items-center">
                  <span className="text-sm">Last System Update</span>
                  <span className="text-xs">Today, 04:30 AM</span>
                </div>
                <div className="flex justify-between items-center mt-1">
                  <span className="text-sm">Last Backup</span>
                  <span className="text-xs">Today, 02:15 AM</span>
                </div>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>
    </div>
  )
}
