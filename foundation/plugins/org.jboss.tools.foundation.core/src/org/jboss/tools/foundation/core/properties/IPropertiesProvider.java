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
package org.jboss.tools.foundation.core.properties;


/**
 * Project/Product properties provider
 *
 * @author Fred Bricon
 * @since 1.1.0
 */
public interface IPropertiesProvider {

  /**
   * Searches for the property with the specified key in this {@link IPropertiesProvider}.
   * If the key is not found the method returns <code>null</code>.
   *
   * @param key   the property key.
   * @return a property matching the key or <code>null</code> if nothing was found.
   */
  String getValue(String key);

  /**
   * Searches for the property with the specified key in this
   * {@link IPropertiesProvider}. If the key is not found the method returns the
   * <code>defaultValue</code>.
   *
   * @param key
   *          the property key.
   * @param defaultValue
   *          the default value that'll be returned if nothing matches the key
   * @return a property matching the key or <code>defaultValue</code> if nothing
   *         was found.
   */
  String getValue(String key, String defaultValue);
}
