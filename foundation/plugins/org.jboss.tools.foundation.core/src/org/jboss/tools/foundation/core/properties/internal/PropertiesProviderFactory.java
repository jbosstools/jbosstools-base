/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.core.properties.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.foundation.core.FoundationCorePlugin;
import org.jboss.tools.foundation.core.properties.IPropertiesProvider;

/**
 * Factory for {@link IPropertiesProvider}s. This factory loads {@link IPropertiesProvider} from the Extension Registry.
 * <code>propertiesProvider</code> found in <code>plugin.xml</code> or <code>fragment.xml</code> are sorted by priority
 * (lowest value has highest priority).
 *
 * @author Fred Bricon
 * @since 1.1.0
 */
public class PropertiesProviderFactory {

  private static final String PROPERTIES_PROVIDERS_EXTENSION_ID = FoundationCorePlugin.PLUGIN_ID+".propertiesProviders";

  private static final String PROPERTIES_PROVIDER_KEY = "propertiesProvider";

  private static final String ATTR_CLASS = "class";

  private  IPropertiesProvider propertiesProvider;

  /**
   * loads {@link IPropertiesProvider} from the Extension Registry. <code>propertiesProvider</code> found in <code>plugin.xml</code> or <code>fragment.xml</code> are sorted by priority
   * (lowest value has highest priority).
   */
  List<IPropertiesProvider> loadPropertiesProviders() {
    IExtensionRegistry registry = Platform.getExtensionRegistry();
    IExtensionPoint extensionPoint = registry.getExtensionPoint(PROPERTIES_PROVIDERS_EXTENSION_ID);
    IExtension[] extensions = extensionPoint.getExtensions();
    final Map<IPropertiesProvider, Integer> providersMap = new HashMap<IPropertiesProvider, Integer>(extensions.length);
    for (IExtension extension : extensions) {
      for (IConfigurationElement element : extension.getConfigurationElements()) {
        if (PROPERTIES_PROVIDER_KEY.equals(element.getName())) {
          try {
            IPropertiesProvider provider = (IPropertiesProvider) element.createExecutableExtension(ATTR_CLASS);
            Integer priority = null;
            try {
              String prio = element.getAttribute("priority");
              if (prio != null) {
                priority = Integer.valueOf(prio);
              }
            } catch (NumberFormatException nfe) {
              FoundationCorePlugin.pluginLog().logError(
                  "Invalid format for IPropertiesProvider priority", nfe);
            }
            if (priority == null) {
              priority = Integer.valueOf(100);
            }
            providersMap.put(provider, priority);
          } catch (Exception e) {
            FoundationCorePlugin.pluginLog().logError("Unable to instanciate propertiesProvider", e);
            continue;
          }
          break;
        }
      }
    }
    List<IPropertiesProvider> providers = new ArrayList<IPropertiesProvider>(providersMap.keySet());
    Collections.sort(providers, new Comparator<IPropertiesProvider>() {
      @Override
      public int compare(IPropertiesProvider p1, IPropertiesProvider p2) {
        return providersMap.get(p2).compareTo(providersMap.get(p1));
      }
    });

    return providers;
  }

  /**
   *  Returns the {@link IPropertiesProvider} with the highest priority from the Extension Registry.
   *  The same instance is always returned.
   */
  public synchronized IPropertiesProvider getPropertiesProvider() {
    if (propertiesProvider == null) {
      List<? extends IPropertiesProvider> providers = loadPropertiesProviders();
      if (!providers.isEmpty()) {
        propertiesProvider = providers.get(0);
      }
    }
    return propertiesProvider;
  }


}
