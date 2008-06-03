/*******************************************************************************
 * Copyright (c) 2005 IBM Corporation and others.
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
package org.jboss.tools.jst.jsp.preferences.xpl;

import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.wst.xml.core.internal.XMLCorePlugin;
import org.eclipse.wst.xml.core.internal.preferences.XMLCorePreferenceNames;
import org.eclipse.wst.xml.ui.internal.XMLUIPlugin;

/**
 * @author Jeremy
 */
public class XMLOccurrencePreferenceConstants implements OccurrencePreferenceConstants {
	/**
	 * A named preference that stores the value for XML elements folding for the folding provider.
	 * <p>
	 * Value is of type <code>Boolean</code>.
	 * </p>
	 * 
	 * @since 3.0
	 */
	public static final String EDITOR_MARK_NODE_OCCURRENCES = "editor_mark_node_occurences"; //$NON-NLS-1$
	public static final String EDITOR_MARK_ATTRIBUTE_OCCURRENCES = "editor_mark_attribute_occurrences"; //$NON-NLS-1$
	public static final String EDITOR_MARK_ATTRIBUTE_VALUE_OCCURRENCES = "editor_mark_attribute_value_occurrences"; //$NON-NLS-1$
	public static final String EDITOR_MARK_TEXT_OCCURRENCES = "editor_mark_text_occurrences"; //$NON-NLS-1$

	
//	fMarkOccurrenceAnnotations= store.getBoolean(ProjectionPreferenceConstants.EDITOR_MARK_OCCURRENCES);
//	fStickyOccurrenceAnnotations= store.getBoolean(ProjectionPreferenceConstants.EDITOR_STICKY_OCCURRENCES);
//	fMarkNodeOccurrences= store.getBoolean(ProjectionPreferenceConstants.EDITOR_MARK_NODE_OCCURRENCES);
//	fMarkAttributeOccurrences= store.getBoolean(ProjectionPreferenceConstants.EDITOR_MARK_ATTRIBUTE_OCCURRENCES);
//	fMarkAttributeValueOccurrences= store.getBoolean(ProjectionPreferenceConstants.EDITOR_MARK_ATTRIBUTE_VALUE_OCCURRENCES);

	/**
	 * Initializes the given preference store with the default values.
	 * 
	 * @param store the preference store to be initialized
	 * 
	 * @since 2.1
	 */
	public static void initializeDefaultValues(IPreferenceStore store) {
		// set the default values from AbstractDecoratedTextEditor

		PreferenceConverter.setDefault(store, "occurrenceIndicationColor", new RGB(128, 255, 255));
		store.setDefault(PreferenceKeyGenerator.generateKey(EDITOR_MARK_OCCURRENCES,XMLUIPlugin.ID), false);
		store.setDefault(PreferenceKeyGenerator.generateKey(EDITOR_STICKY_OCCURRENCES,XMLUIPlugin.ID), true);
		store.setDefault(PreferenceKeyGenerator.generateKey(EDITOR_OCCURRENCE_PROVIDER,XMLUIPlugin.ID), "org.jboss.tools.common.text.xml.defaultOccurrenceProvider"); //$NON-NLS-1$

		store.setDefault(PreferenceKeyGenerator.generateKey(EDITOR_MARK_NODE_OCCURRENCES,XMLUIPlugin.ID), true);
		store.setDefault(PreferenceKeyGenerator.generateKey(EDITOR_MARK_ATTRIBUTE_OCCURRENCES,XMLUIPlugin.ID), true);
		store.setDefault(PreferenceKeyGenerator.generateKey(EDITOR_MARK_ATTRIBUTE_VALUE_OCCURRENCES,XMLUIPlugin.ID), true);
		store.setDefault(PreferenceKeyGenerator.generateKey(EDITOR_MARK_TEXT_OCCURRENCES,XMLUIPlugin.ID), true);
	}

	public static boolean affectsPreferences(String property) {
		return (PreferenceKeyGenerator.generateKey(EDITOR_MARK_OCCURRENCES,XMLUIPlugin.ID).equals(property) ||
			PreferenceKeyGenerator.generateKey(EDITOR_STICKY_OCCURRENCES,XMLUIPlugin.ID).equals(property) ||
			PreferenceKeyGenerator.generateKey(EDITOR_OCCURRENCE_PROVIDER,XMLUIPlugin.ID).equals(property) ||
			PreferenceKeyGenerator.generateKey(EDITOR_MARK_NODE_OCCURRENCES,XMLUIPlugin.ID).equals(property) ||
			PreferenceKeyGenerator.generateKey(EDITOR_MARK_ATTRIBUTE_OCCURRENCES,XMLUIPlugin.ID).equals(property) ||
			PreferenceKeyGenerator.generateKey(EDITOR_MARK_ATTRIBUTE_VALUE_OCCURRENCES,XMLUIPlugin.ID).equals(property) ||
			PreferenceKeyGenerator.generateKey(EDITOR_MARK_TEXT_OCCURRENCES,XMLUIPlugin.ID).equals(property));
	}
}
