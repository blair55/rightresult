workbox.core.skipWaiting();
workbox.core.clientsClaim();

self.addEventListener('push', event => {
  console.log(event.data.text());
  const options = {
    body: event.data.text(),
    icon: '/logo.png',
    badge: '/notify-badge.png'
  };
  event.waitUntil(self.registration.showNotification('Right Result', options));
});

self.addEventListener('notificationclick', function (event) {
  const clickedNotification = event.notification;
  clickedNotification.close();
  event.waitUntil(clients.openWindow('/fixtures'));
});

workbox.precaching.precacheAndRoute(self.__precacheManifest);