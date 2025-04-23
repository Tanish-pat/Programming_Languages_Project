import { Avatar, AvatarFallback } from "@/components/ui/avatar"
import { Package, User, CreditCard, Tag, AlertCircle } from "lucide-react"

const activities = [
  {
    id: 1,
    type: "product",
    action: "New product added",
    name: "Wireless Headphones",
    time: "2 minutes ago",
  },
  {
    id: 2,
    type: "customer",
    action: "New customer registered",
    name: "John Smith",
    time: "15 minutes ago",
  },
  {
    id: 3,
    type: "payment",
    action: "Payment received",
    name: "$129.99",
    time: "1 hour ago",
  },
  {
    id: 4,
    type: "inventory",
    action: "Low stock alert",
    name: "Smartphone Case",
    time: "2 hours ago",
  },
  {
    id: 5,
    type: "coupon",
    action: "New coupon created",
    name: "SUMMER25",
    time: "3 hours ago",
  },
]

export function RecentActivityTable() {
  const getIcon = (type: string) => {
    switch (type) {
      case "product":
        return <Package className="h-4 w-4" />
      case "customer":
        return <User className="h-4 w-4" />
      case "payment":
        return <CreditCard className="h-4 w-4" />
      case "coupon":
        return <Tag className="h-4 w-4" />
      case "inventory":
        return <AlertCircle className="h-4 w-4" />
      default:
        return <Package className="h-4 w-4" />
    }
  }

  const getColor = (type: string) => {
    switch (type) {
      case "product":
        return "bg-blue-500"
      case "customer":
        return "bg-green-500"
      case "payment":
        return "bg-purple-500"
      case "coupon":
        return "bg-yellow-500"
      case "inventory":
        return "bg-red-500"
      default:
        return "bg-gray-500"
    }
  }

  return (
    <div className="space-y-4">
      {activities.map((activity) => (
        <div key={activity.id} className="flex items-center gap-4">
          <Avatar className={`h-9 w-9 ${getColor(activity.type)}`}>
            <AvatarFallback className="text-white">{getIcon(activity.type)}</AvatarFallback>
          </Avatar>
          <div className="flex-1 space-y-1">
            <p className="text-sm font-medium leading-none">{activity.action}</p>
            <p className="text-sm text-muted-foreground">{activity.name}</p>
          </div>
          <div className="text-xs text-muted-foreground">{activity.time}</div>
        </div>
      ))}
    </div>
  )
}
