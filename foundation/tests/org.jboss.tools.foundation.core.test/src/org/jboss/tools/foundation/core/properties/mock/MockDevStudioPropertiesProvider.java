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
package org.jboss.tools.foundation.core.properties.mock;

import org.jboss.tools.foundation.core.properties.internal.VersionPropertiesProvider;

/**
 * Properties Provider for mocking JBoss Developer Studio
 *
 * @author Fred Bricon
 */
public class MockDevStudioPropertiesProvider extends VersionPropertiesProvider {

  public MockDevStudioPropertiesProvider() {
    super();
    initPropertiesUri("file://crap.url");
  }

  public String getCurrentVersion() {
	  return "8.0.0";
  };
  
  @Override
  public String getContext() {
	return "devstudio";
  }
}
