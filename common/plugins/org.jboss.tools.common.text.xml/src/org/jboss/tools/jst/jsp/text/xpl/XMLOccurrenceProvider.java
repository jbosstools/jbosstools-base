/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc.
 *******************************************************************************/
package org.jboss.tools.jst.jsp.text.xpl;

import org.eclipse.wst.xml.ui.internal.XMLUIPlugin;

import org.jboss.tools.jst.jsp.preferences.xpl.XMLOccurrencePreferenceConstants;

/**
 * @author Jeremy
 *
 */
public class XMLOccurrenceProvider extends DefaultStructuredTextOccurrenceStructureProvider {
	
	public XMLOccurrenceProvider () {
		super (XMLUIPlugin.ID, XMLUIPlugin.getDefault().getPreferenceStore());
	}

	public boolean affectsPreferences(String property) {
		return XMLOccurrencePreferenceConstants.affectsPreferences(property);
	}

}
