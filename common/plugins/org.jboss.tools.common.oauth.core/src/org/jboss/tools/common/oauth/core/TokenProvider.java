/*******************************************************************************
 * Copyright (c) 2021 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.oauth.core;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.oauth.core.exception.OAuthException;
import org.jboss.tools.common.oauth.internal.core.CommonOAuthCoreActivator;

/**
 * Interface to be implemented for the tokenProvider extension point.
 */
public interface TokenProvider {
  public static final int ID_TOKEN = 0;
  public static final int ACCESS_TOKEN = 1;
  public static final int REFRESH_TOKEN = 2;
  
	String getToken(String serverId, int tokentype, Object context) throws OAuthException;
	
	default String getToken(String serverId) throws OAuthException {
	  return getToken(serverId, ID_TOKEN, null);
	}

	public static TokenProvider get() {
		IConfigurationElement[] elements = Platform.getExtensionRegistry()
				.getConfigurationElementsFor(CommonOAuthCoreConstants.TOKEN_PROVIDER_EXTENSION_POINT);
		for (IConfigurationElement element : elements) {
			try {
				return (TokenProvider) element.createExecutableExtension("class");
			} catch (CoreException e) {
				CommonOAuthCoreActivator.logError(e.getLocalizedMessage(), e);
			}
		}
		return null;
	}
}
