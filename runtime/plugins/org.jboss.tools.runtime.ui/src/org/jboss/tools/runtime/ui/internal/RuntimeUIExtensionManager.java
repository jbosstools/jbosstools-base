/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.runtime.ui.internal;

import java.util.HashMap;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeAuthenticator;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

/**
 * A class in charge of loading any and all 
 * extensions for this plugin. 
 * 
 */
public class RuntimeUIExtensionManager {
	public static final String DOWNLOAD_RUNTIMES_UI_EXTENSION_ID = "org.jboss.tools.runtime.ui.downloadRuntimeAuthenticatorUI"; //$NON-NLS-1$

	private static RuntimeUIExtensionManager singleton;
	public static RuntimeUIExtensionManager getDefault() {
		if( singleton == null )
			singleton = new RuntimeUIExtensionManager();
		return singleton;
	}
	
	private HashMap<String, AuthenticatorUIWrapper> map = null;	
	public WizardFragment getAuthenticatorUI(IDownloadRuntimeAuthenticator auth) {
		return auth == null ? null : getAuthenticatorUI(auth.getAuthenticatorId());
	}
	
	public WizardFragment getAuthenticatorUI(String authenticatorId) {
		if( authenticatorId == null )
			return null;
		if( map == null ) {
			loadAuthenticatorUIExtensions();
		}
		AuthenticatorUIWrapper wrapper = map.get(authenticatorId);
		if( wrapper != null ) {
			try {
				return wrapper.createWizardFragment();
			} catch(CoreException ce) {
				RuntimeUIActivator.pluginLog().logError(ce);
			}
		}
		return null;
	}
	
	private synchronized void loadAuthenticatorUIExtensions() {
		HashMap<String, AuthenticatorUIWrapper> tmp = new HashMap<String, AuthenticatorUIWrapper>();
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = registry
				.getExtensionPoint(DOWNLOAD_RUNTIMES_UI_EXTENSION_ID);
		IExtension[] extensions = extensionPoint.getExtensions();
		for (int i = 0; i < extensions.length; i++) {
			IExtension extension = extensions[i];
			IConfigurationElement[] configurationElements = extension
					.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				AuthenticatorUIWrapper dec = loadOneAuthenticatorUI(configurationElements[j]); 
				Trace.trace(Trace.STRING_EXTENSION_POINT, "Loaded authenticator UI wrapper for authenticator id " + dec.getAuthenticatorId()); //$NON-NLS-1$
				tmp.put(dec.getAuthenticatorId(), dec);
			}
		}
		map = tmp;
	}
	
	private AuthenticatorUIWrapper loadOneAuthenticatorUI(IConfigurationElement el) {
		return new AuthenticatorUIWrapper(el);
	}
}
