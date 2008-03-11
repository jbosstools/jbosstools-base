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
package org.jboss.tools.common.model.ui.views.palette;

/*
 * Temporary solution to prevent focus losing when inserting from palette using wizard
 * Used in JSP TextEditor and PaletteInsertHelper
 * When we insert tag from palette using spechial wizards JSP Editor losts focus and Outline change selection. It's bad.
 */
public interface IIgnoreSelection {
	/*
	 * Return true if insertion is from palette wizard 
	 */
	public boolean doesIgnore();
	public void setIgnore(boolean ignore);
}