/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.meta.ui;

import org.jboss.tools.common.log.BaseUIPlugin;

/**
 * @author eskimo
 *
 */
public class MetaUIPlugin extends BaseUIPlugin {

	private static MetaUIPlugin plugin;
	/**
	 * 
	 */
	public MetaUIPlugin() {
		plugin = this;
	}
	
	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static MetaUIPlugin getDefault() {
		return plugin;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.log.BaseUIPlugin#getId()
	 */
	@Override
	public String getId() {
		return "org.jboss.tools.common.meta.ui";
	}
}