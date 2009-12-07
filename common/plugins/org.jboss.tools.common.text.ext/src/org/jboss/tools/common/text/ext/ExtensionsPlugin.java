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
package org.jboss.tools.common.text.ext;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;
import org.jboss.tools.common.log.BaseUIPlugin;
import org.jboss.tools.common.log.IPluginLog;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkDetector;


/**
 * @author Jeremy
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class ExtensionsPlugin extends BaseUIPlugin implements IAdaptable {
	//The shared instance.
	private static ExtensionsPlugin plugin;
	
	public static final String PLUGIN_ID = "org.jboss.tools.common.text.ext";  //$NON-NLS-1$

	/**
	 * The constructor.
	 */
    public ExtensionsPlugin() {
        super();
        plugin = this;
    }

    /**
	 * Returns the shared instance.
	 */
	public static ExtensionsPlugin getDefault() {
		return plugin;
	}

	public Object getAdapter(Class adapter) {
		if (adapter == IHyperlinkDetector.class) {
			return HyperlinkDetector.getInstance();
		}
		return null;
	}

	/**
	 * @return IPluginLog object
	 */
	public static IPluginLog getPluginLog() {
		return getDefault();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.log.BaseUIPlugin#getId()
	 */
	@Override
	public String getId() {
		return PLUGIN_ID;
	}
}