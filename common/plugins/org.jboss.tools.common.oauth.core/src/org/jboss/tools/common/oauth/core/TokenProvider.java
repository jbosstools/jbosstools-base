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
	String getToken(String serverId, Object context) throws OAuthException;
	
	default String getToken(String serverId) throws OAuthException {
	  return getToken(serverId, null);
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
