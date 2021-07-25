workbox.core.skipWaiting();
workbox.core.clientsClaim();

self.addEventListener('push', event => {
  var notification = event.data.json();
  const options = {
    body: notification.Body,
    icon: '/logo.png',
    badge: '/notify-badge.png'
  };
  event.waitUntil(self.registration.showNotification(notification.Title, options));
});

self.addEventListener('notificationclick', function (event) {
  const clickedNotification = event.notification;
  clickedNotification.close();
  event.waitUntil(clients.openWindow('/fixtures'));
});

workbox.precaching.precacheAndRoute(self.__WB_MANIFEST);