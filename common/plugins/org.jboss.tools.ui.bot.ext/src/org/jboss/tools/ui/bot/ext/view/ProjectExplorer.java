/*******************************************************************************
 * Copyright (c) 2007-2009 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.ui.bot.ext.view;

import org.jboss.tools.ui.bot.ext.gen.ActionItem.View;

/**
 * Eclipse project explorer helper component
 * 
 * @author jpeterka & lzoubek
 * 
 */
public class ProjectExplorer extends ExplorerBase {

	public ProjectExplorer() {
		viewObject = View.GeneralProjectExplorer.LABEL;
	}
}
