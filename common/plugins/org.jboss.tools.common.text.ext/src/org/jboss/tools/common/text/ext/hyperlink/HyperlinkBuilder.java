/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext.hyperlink;

import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.xpl.BaseHyperlinkBuilder;

/**
 * 
 * @author eskimo(dgolovin@exadel.com)
 *
 */
public class HyperlinkBuilder extends BaseHyperlinkBuilder {

	/**
	 * 
	 * @param tag
	 * @param extensionPoint
	 */
	protected void readContributions(String tag, String extensionPoint) {
		targetContributionTag = tag;
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		readRegistry(registry, ExtensionsPlugin.PLUGIN_ID, extensionPoint);
	}	
}