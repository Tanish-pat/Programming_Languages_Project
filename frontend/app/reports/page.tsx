import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { SalesReport } from "@/components/reports/sales-report"
import { InventoryReport } from "@/components/reports/inventory-report"
import { CategoryReport } from "@/components/reports/category-report"

export default function ReportsPage() {
  return (
    <div className="flex-1 space-y-4 p-4 pt-6 md:p-8">
      <div className="flex items-center justify-between">
        <h2 className="text-3xl font-bold tracking-tight">Reports</h2>
      </div>

      <Tabs defaultValue="sales" className="space-y-4">
        <TabsList>
          <TabsTrigger value="sales">Sales</TabsTrigger>
          <TabsTrigger value="inventory">Inventory</TabsTrigger>
          <TabsTrigger value="categories">Categories</TabsTrigger>
        </TabsList>
        <TabsContent value="sales" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Sales Report</CardTitle>
              <CardDescription>View your sales performance over time</CardDescription>
            </CardHeader>
            <CardContent className="pl-2">
              <SalesReport />
            </CardContent>
          </Card>
        </TabsContent>
        <TabsContent value="inventory" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Inventory Report</CardTitle>
              <CardDescription>Analyze your inventory levels and movement</CardDescription>
            </CardHeader>
            <CardContent>
              <InventoryReport />
            </CardContent>
          </Card>
        </TabsContent>
        <TabsContent value="categories" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Category Report</CardTitle>
              <CardDescription>View sales and inventory by category</CardDescription>
            </CardHeader>
            <CardContent>
              <CategoryReport />
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  )
}
