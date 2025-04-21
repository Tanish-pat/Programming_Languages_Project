"use client"

import { useEffect, useState } from "react"
import Link from "next/link"
import { ArrowDown, ArrowUp, Clock, Edit, Package, Plus, Trash, Truck, User } from "lucide-react"

import { Avatar, AvatarFallback } from "@/components/ui/avatar"
import { Skeleton } from "@/components/ui/skeleton"
import { fetchRecentActivity } from "@/lib/api"
import { formatDate } from "@/lib/utils"

interface Activity {
  id: number
  type: "add" | "edit" | "delete" | "restock" | "order" | "supplier"
  entityType: "item" | "category" | "supplier" | "order" | "customer"
  entityId: number
  entityName: string
  user: string
  timestamp: string
  details?: string
}

export function RecentActivity() {
  const [activities, setActivities] = useState<Activity[]>([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    const getActivities = async () => {
      try {
        // Fetch recent activity from the API
        const data = await fetchRecentActivity()
        setActivities(data)
      } catch (error) {
        console.error("Failed to fetch recent activity:", error)
      } finally {
        setLoading(false)
      }
    }

    getActivities()
  }, [])

  if (loading) {
    return (
      <div className="space-y-4">
        {Array(5)
          .fill(0)
          .map((_, i) => (
            <div key={i} className="flex items-start gap-4">
              <Skeleton className="h-10 w-10 rounded-full" />
              <div className="space-y-2">
                <Skeleton className="h-4 w-[250px]" />
                <Skeleton className="h-4 w-[200px]" />
              </div>
            </div>
          ))}
      </div>
    )
  }

  const getActivityIcon = (activity: Activity) => {
    switch (activity.type) {
      case "add":
        return <Plus className="h-4 w-4" />
      case "edit":
        return <Edit className="h-4 w-4" />
      case "delete":
        return <Trash className="h-4 w-4" />
      case "restock":
        return <ArrowUp className="h-4 w-4" />
      case "order":
        return <ArrowDown className="h-4 w-4" />
      case "supplier":
        return <Truck className="h-4 w-4" />
      default:
        return <Clock className="h-4 w-4" />
    }
  }

  const getActivityColor = (activity: Activity) => {
    switch (activity.type) {
      case "add":
        return "bg-green-100 text-green-600 dark:bg-green-900/20 dark:text-green-400"
      case "edit":
        return "bg-blue-100 text-blue-600 dark:bg-blue-900/20 dark:text-blue-400"
      case "delete":
        return "bg-red-100 text-red-600 dark:bg-red-900/20 dark:text-red-400"
      case "restock":
        return "bg-purple-100 text-purple-600 dark:bg-purple-900/20 dark:text-purple-400"
      case "order":
        return "bg-amber-100 text-amber-600 dark:bg-amber-900/20 dark:text-amber-400"
      case "supplier":
        return "bg-cyan-100 text-cyan-600 dark:bg-cyan-900/20 dark:text-cyan-400"
      default:
        return "bg-gray-100 text-gray-600 dark:bg-gray-800 dark:text-gray-400"
    }
  }

  const getEntityIcon = (activity: Activity) => {
    switch (activity.entityType) {
      case "item":
        return <Package className="h-4 w-4" />
      case "supplier":
        return <Truck className="h-4 w-4" />
      case "customer":
        return <User className="h-4 w-4" />
      default:
        return <Package className="h-4 w-4" />
    }
  }

  const getActivityText = (activity: Activity) => {
    switch (activity.type) {
      case "add":
        return `added a new ${activity.entityType}`
      case "edit":
        return `updated ${activity.entityType} details`
      case "delete":
        return `removed a ${activity.entityType}`
      case "restock":
        return `restocked inventory item`
      case "order":
        return `created a new order`
      case "supplier":
        return `added a new supplier`
      default:
        return `performed an action on ${activity.entityType}`
    }
  }

  return (
    <div className="space-y-6">
      {activities.length === 0 ? (
        <p className="text-center text-muted-foreground">No recent activity</p>
      ) : (
        activities.map((activity) => (
          <div key={activity.id} className="flex items-start gap-4">
            <Avatar className="h-10 w-10">
              <AvatarFallback className="bg-primary/10 text-primary">
                {activity.user.substring(0, 2).toUpperCase()}
              </AvatarFallback>
            </Avatar>
            <div className="space-y-1">
              <div className="flex items-center gap-2">
                <span className="font-medium">{activity.user}</span>
                <span className="text-muted-foreground">{getActivityText(activity)}</span>
                <Link href={`/${activity.entityType}s/${activity.entityId}`} className="font-medium hover:underline">
                  {activity.entityName}
                </Link>
              </div>
              <div className="flex items-center gap-2 text-sm text-muted-foreground">
                <div className={`flex h-6 w-6 items-center justify-center rounded-full ${getActivityColor(activity)}`}>
                  {getActivityIcon(activity)}
                </div>
                <span>{formatDate(activity.timestamp)}</span>
                {activity.details && <span>• {activity.details}</span>}
              </div>
            </div>
          </div>
        ))
      )}
    </div>
  )
}
