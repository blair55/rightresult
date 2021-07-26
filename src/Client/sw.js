import { clientsClaim } from 'workbox-core';
import { cleanupOutdatedCaches, precacheAndRoute } from 'workbox-precaching';

self.skipWaiting()
clientsClaim();

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

cleanupOutdatedCaches();
precacheAndRoute(self.__WB_MANIFEST);