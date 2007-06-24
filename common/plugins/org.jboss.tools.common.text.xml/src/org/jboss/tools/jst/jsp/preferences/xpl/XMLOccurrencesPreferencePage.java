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

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.wst.xml.ui.internal.XMLUIPlugin;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.jboss.tools.common.text.xml.XmlEditorPlugin;

/**
 * @author tau
 *
 */
public class XMLOccurrencesPreferencePage extends FieldEditorPreferencePage
		implements IWorkbenchPreferencePage {

	private BooleanFieldEditor mark;

	private VarBooleanFieldEditor markNode;

	private VarBooleanFieldEditor markAttribute;

	private VarBooleanFieldEditor markAttributeValue;

	private VarBooleanFieldEditor markText;

	private VarBooleanFieldEditor sticky;

	private String fEditorID;

	private IPreferenceStore fPreferenceStore;

	public XMLOccurrencesPreferencePage(String editorID, IPreferenceStore store) {
		// Set the preference store for the preference page.
		super(FieldEditorPreferencePage.GRID);
		fEditorID = editorID;
		fPreferenceStore = store;
		setPreferenceStore(store);
	}

	public XMLOccurrencesPreferencePage() {
		this(XMLUIPlugin.ID, XMLUIPlugin.getDefault().getPreferenceStore()); // add
																				// tau
																				// 02.02.2005
	}

	public void initializeDefaultValues() {
		XMLOccurrencePreferenceConstants
				.initializeDefaultValues(fPreferenceStore);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors()
	 */
	protected void createFieldEditors() {
		// Initialize all field editors.
		try {
			mark = new BooleanFieldEditor(
					PreferenceKeyGenerator
							.generateKey(
									XMLOccurrencePreferenceConstants.EDITOR_MARK_OCCURRENCES,
									fEditorID),
					Messages
							.getString("OccurrencesPreferencePage.Mark.occurrences.in.file"),
					getFieldEditorParent()); //$NON-NLS-1$
			addField(mark);

			markNode = new VarBooleanFieldEditor(
					PreferenceKeyGenerator
							.generateKey(
									XMLOccurrencePreferenceConstants.EDITOR_MARK_NODE_OCCURRENCES,
									fEditorID), Messages
							.getString("OccurrencesPreferencePage.Mark.Tags"),
					getFieldEditorParent()); //$NON-NLS-1$
			markNode.offset(getFieldEditorParent(), 10); // offset 10
			addField(markNode);

			markAttribute = new VarBooleanFieldEditor(
					PreferenceKeyGenerator
							.generateKey(
									XMLOccurrencePreferenceConstants.EDITOR_MARK_ATTRIBUTE_OCCURRENCES,
									fEditorID),
					Messages
							.getString("OccurrencesPreferencePage.Mark.Attribute.Names"),
					getFieldEditorParent()); //$NON-NLS-1$
			markAttribute.offset(getFieldEditorParent(), 10); // offset 10
			addField(markAttribute);

			markAttributeValue = new VarBooleanFieldEditor(
					PreferenceKeyGenerator
							.generateKey(
									XMLOccurrencePreferenceConstants.EDITOR_MARK_ATTRIBUTE_VALUE_OCCURRENCES,
									fEditorID),
					Messages
							.getString("OccurrencesPreferencePage.Mark.Attribute.Values"),
					getFieldEditorParent()); //$NON-NLS-1$
			markAttributeValue.offset(getFieldEditorParent(), 10); // offset 10
			addField(markAttributeValue);

			markText = new VarBooleanFieldEditor(
					PreferenceKeyGenerator
							.generateKey(
									XMLOccurrencePreferenceConstants.EDITOR_MARK_TEXT_OCCURRENCES,
									fEditorID), Messages
							.getString("OccurrencesPreferencePage.Mark.Text"),
					getFieldEditorParent());
			markText.offset(getFieldEditorParent(), 10); // offset 10
			addField(markText);

			FieldEditor spacer1 = new LabelFieldEditor(
					"", getFieldEditorParent()); //$NON-NLS-1$
			addField(spacer1);

			sticky = new VarBooleanFieldEditor(
					PreferenceKeyGenerator
							.generateKey(
									XMLOccurrencePreferenceConstants.EDITOR_STICKY_OCCURRENCES,
									fEditorID), Messages
							.getString("OccurrencesPreferencePage.Sticky"),
					getFieldEditorParent()); //$NON-NLS-1$
			sticky.offset(getFieldEditorParent(), 10); // offset 10
			addField(sticky);
		} catch (Exception x) {
			x.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse.jface.util.PropertyChangeEvent)
	 */
	public void propertyChange(PropertyChangeEvent event) {
		super.propertyChange(event);
		BooleanFieldEditor booleanFieldEditor = (BooleanFieldEditor)event.getSource();
		
		if ( booleanFieldEditor.getPreferenceName().equals(
				PreferenceKeyGenerator.generateKey(XMLOccurrencePreferenceConstants.EDITOR_MARK_OCCURRENCES,fEditorID))) {
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.FieldEditorPreferencePage#initialize()
	 */
	protected void initialize() {
		super.initialize();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
	 */
	protected void performDefaults() {
		super.performDefaults();
		mark.setEnabled(true, getFieldEditorParent());
	}
}
