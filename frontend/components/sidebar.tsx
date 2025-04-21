"use client"

import type React from "react"

import Link from "next/link"
import { usePathname } from "next/navigation"
import { BarChart3, BoxIcon, Home, Package, Settings, ShoppingCart, Truck, Users } from "lucide-react"

import { cn } from "@/lib/utils"
import { Button } from "@/components/ui/button"
import { ScrollArea } from "@/components/ui/scroll-area"
import { Sheet, SheetContent, SheetTrigger } from "@/components/ui/sheet"

interface SidebarProps extends React.HTMLAttributes<HTMLDivElement> {
  isCollapsed?: boolean
}

export function Sidebar({ className, isCollapsed = false }: SidebarProps) {
  const pathname = usePathname()

  const routes = [
    {
      label: "Dashboard",
      icon: Home,
      href: "/",
      active: pathname === "/",
    },
    {
      label: "Inventory",
      icon: Package,
      href: "/inventory",
      active: pathname === "/inventory" || pathname.startsWith("/inventory/"),
    },
    {
      label: "Categories",
      icon: BoxIcon,
      href: "/categories",
      active: pathname === "/categories" || pathname.startsWith("/categories/"),
    },
    {
      label: "Suppliers",
      icon: Truck,
      href: "/suppliers",
      active: pathname === "/suppliers" || pathname.startsWith("/suppliers/"),
    },
    {
      label: "Orders",
      icon: ShoppingCart,
      href: "/orders",
      active: pathname === "/orders" || pathname.startsWith("/orders/"),
    },
    {
      label: "Customers",
      icon: Users,
      href: "/customers",
      active: pathname === "/customers" || pathname.startsWith("/customers/"),
    },
    {
      label: "Reports",
      icon: BarChart3,
      href: "/reports",
      active: pathname === "/reports" || pathname.startsWith("/reports/"),
    },
    {
      label: "Settings",
      icon: Settings,
      href: "/settings",
      active: pathname === "/settings" || pathname.startsWith("/settings/"),
    },
  ]

  return (
    <>
      {/* Desktop Sidebar */}
      <aside
        className={cn("hidden h-screen border-r bg-background lg:block", isCollapsed ? "w-16" : "w-64", className)}
      >
        <div className="flex h-14 items-center border-b px-4">
          <Link href="/" className="flex items-center gap-2 font-semibold">
            <Package className="h-6 w-6" />
            {!isCollapsed && <span>Inventory System</span>}
          </Link>
        </div>
        <ScrollArea className="h-[calc(100vh-3.5rem)]">
          <div className="px-3 py-2">
            <nav className="grid gap-1">
              {routes.map((route) => (
                <Link key={route.href} href={route.href} passHref>
                  <Button
                    variant={route.active ? "secondary" : "ghost"}
                    className={cn(
                      "w-full justify-start",
                      isCollapsed ? "px-2" : "px-3",
                      route.active ? "font-medium" : "font-normal",
                    )}
                  >
                    <route.icon className={cn("h-5 w-5", isCollapsed ? "mr-0" : "mr-2")} />
                    {!isCollapsed && <span>{route.label}</span>}
                  </Button>
                </Link>
              ))}
            </nav>
          </div>
        </ScrollArea>
      </aside>

      {/* Mobile Sidebar */}
      <Sheet>
        <SheetTrigger asChild>
          <Button variant="outline" size="icon" className="lg:hidden">
            <Package className="h-5 w-5" />
            <span className="sr-only">Toggle Menu</span>
          </Button>
        </SheetTrigger>
        <SheetContent side="left" className="w-64 p-0">
          <div className="flex h-14 items-center border-b px-4">
            <Link href="/" className="flex items-center gap-2 font-semibold">
              <Package className="h-6 w-6" />
              <span>Inventory System</span>
            </Link>
          </div>
          <ScrollArea className="h-[calc(100vh-3.5rem)]">
            <div className="px-3 py-2">
              <nav className="grid gap-1">
                {routes.map((route) => (
                  <Link key={route.href} href={route.href} passHref>
                    <Button
                      variant={route.active ? "secondary" : "ghost"}
                      className={cn("w-full justify-start px-3", route.active ? "font-medium" : "font-normal")}
                    >
                      <route.icon className="mr-2 h-5 w-5" />
                      <span>{route.label}</span>
                    </Button>
                  </Link>
                ))}
              </nav>
            </div>
          </ScrollArea>
        </SheetContent>
      </Sheet>
    </>
  )
}
