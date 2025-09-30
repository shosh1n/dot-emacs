(setq package-enable-at-startup nil
      frame-resize-pixelwise t
      package-native-compile t)
(require 'plstore)
(add-to-list 'plstore-encrypt-to '("488B0DAA0B420C3CEDC30F8308ADA8CDF127AC03"))
(setq epg-pinentry-mode 'loopback)
