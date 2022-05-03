#### Changes to Auto-configuration

##### Auto-configuration Registration

If you have created your own auto-configurations, you should move the registration from spring.factories to a new file named
META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports. Each line contains the fully qualified name of the auto-configuration.

Loading auto-configurations from spring.factories is deprecated. For backwards compatibility, entries in spring.factories will still be honored.

##### New @AutoConfiguration Annotation

A new @AutoConfiguration annotation has been introduced. It should be used to annotate top-level auto-configuration classes that are listed in the new
META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports file, replacing @Configuration. Configuration classes that are nested within or
imported by an @AutoConfiguration class should continue to use @Configuration as before.

For your convenience, @AutoConfiguration also supports auto-configuration ordering via the after, afterNames, before and beforeNames attributes. This can be
used as a replacement for @AutoConfigureAfter and @AutoConfigureBefore.

#### Test Slice Configuration

If you have created your own test-slices, you should move the registration from spring.factories to the new place under
META-INF/spring/<name of your test slice annotation>.imports. The format is the same as the new file described in the "Auto-configuration Registration" section,
see above.
