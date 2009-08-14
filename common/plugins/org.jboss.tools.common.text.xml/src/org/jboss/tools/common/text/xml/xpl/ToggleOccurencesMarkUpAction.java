/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
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
package org.jboss.tools.common.text.xml.xpl;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.eclipse.wst.sse.ui.StructuredTextEditor;
import org.jboss.tools.common.text.xml.IOccurrencePreferenceProvider;
import org.jboss.tools.common.text.xml.XmlEditorPlugin;
import org.jboss.tools.jst.jsp.preferences.xpl.Messages;
import org.jboss.tools.jst.jsp.preferences.xpl.OccurrencePreferenceConstants;
import org.jboss.tools.jst.jsp.preferences.xpl.PreferenceKeyGenerator;

/**
 * A toolbar action which toggles the presentation model of the
 * connected text editor. The editor shows either the highlight range
 * only or always the whole document.
 */
public class ToggleOccurencesMarkUpAction extends TextEditorAction implements IPropertyChangeListener {
	IPreferenceStore fPreferenceStore;
	IOccurrencePreferenceProvider fOccurrencePreferenceProvider;
	String fEditorId;
	String fKey;

	/**
	 * Constructs and updates the action.
	 */
	public ToggleOccurencesMarkUpAction() {
		super(Messages.getResourceBundle(), "ToggleOccurencesMarkUpAction.", null); //$NON-NLS-1$
	}
	
	/**
	 * Retargets this action to the given editor.
	 *
	 * @param editor the new editor, or <code>null</code> if none
	 */
	public void setEditor(ITextEditor editor) {
		super.setEditor(editor);
		if(editor == null) {
			dispose();
			return;
		}
		fOccurrencePreferenceProvider = null;
		StructuredTextEditor sse = getStructuredTextEditor(getTextEditor());
		if (sse instanceof IOccurrencePreferenceProvider) {
			fOccurrencePreferenceProvider = (IOccurrencePreferenceProvider)sse;
			if (fOccurrencePreferenceProvider != null && fOccurrencePreferenceProvider.getOccurrencePreferenceProvider() != null) {
				fEditorId = fOccurrencePreferenceProvider.getOccurrencePreferenceProvider().getEditorId();
				fKey = PreferenceKeyGenerator.generateKey(
						OccurrencePreferenceConstants.EDITOR_MARK_OCCURRENCES,
						fEditorId);
			}
		}
		update();
	}

	private StructuredTextEditor getStructuredTextEditor(ITextEditor editor) {
		if (editor == null) return null;
		if (editor instanceof StructuredTextEditor) return (StructuredTextEditor)editor;
		return (StructuredTextEditor)editor.getAdapter(StructuredTextEditor.class);
	}
	
	protected void initialize(ResourceBundle bundle, String prefix) {
		String labelKey= "label"; //$NON-NLS-1$
		String tooltipKey= "tooltip"; //$NON-NLS-1$
		String imageKey= "image"; //$NON-NLS-1$
		String disabledImageKey= "disabledImage"; //$NON-NLS-1$
		String descriptionKey= "description"; //$NON-NLS-1$

		if (prefix != null && prefix.length() > 0) {
			labelKey= prefix + labelKey;
			tooltipKey= prefix + tooltipKey;
			imageKey= prefix + imageKey;
			disabledImageKey= prefix + disabledImageKey;
			descriptionKey= prefix + descriptionKey;
		}

		setText(getString(bundle, labelKey, labelKey));
		setToolTipText(getString(bundle, tooltipKey, null));
		setDescription(getString(bundle, descriptionKey, null));

		String relPath= getString(bundle, imageKey, null);
		ImageDescriptor id = ImageDescriptor.createFromFile(ToggleOccurencesMarkUpAction.class, relPath); //$NON-NLS-1$
		if (id != null) {
			setImageDescriptor(id);
		}
	
		relPath = getString(bundle, disabledImageKey, null);
		id= ImageDescriptor.createFromFile(ToggleOccurencesMarkUpAction.class, relPath); //$NON-NLS-1$
		if (id != null) {
			setDisabledImageDescriptor(id);
		}
	}

	public int getStyle() {
		return AS_RADIO_BUTTON;
	}

	public void run() {
		boolean markOccurences = false;
		// determine if action should be enabled or not
		markOccurences = fPreferenceStore.getBoolean(fKey);
		fPreferenceStore.setValue(fKey, !markOccurences);
	}

	static int listenerCount = 0;
	/**
	 * Enables and initialzies the action, or disables.
	 * @see org.eclipse.ui.texteditor.TextEditorAction#update()
	 */
	public void update() {
		IPreferenceStore newStore = null;
		super.update();
		newStore = ((fOccurrencePreferenceProvider == null 
				|| fOccurrencePreferenceProvider.getOccurrencePreferenceProvider() == null) 
				? null : fOccurrencePreferenceProvider.getOccurrencePreferenceProvider().getPreferenceStore());
		if (newStore != fPreferenceStore) {
			if (fPreferenceStore != null) { 
				fPreferenceStore.removePropertyChangeListener(this);
			}
			fPreferenceStore = newStore;
			if (fPreferenceStore != null) { 
				fPreferenceStore.addPropertyChangeListener(this);
			}
		}
			
		boolean markOccurences = false;
		boolean enabled = false;
		// determine if action should be enabled or not
		if(fPreferenceStore != null) {
			markOccurences = fPreferenceStore.getBoolean(fKey);
		}
		enabled = (getTextEditor() != null);

		setChecked(markOccurences);
		setEnabled(enabled);
	}
	
	/*
	 * @see IPropertyChangeListener#propertyChange(org.eclipse.jface.util.PropertyChangeEvent)
	 */
	public void propertyChange(PropertyChangeEvent event) {
		update();
	}

	private static URL makeIconFileURL(String prefix, String name) throws MalformedURLException {
		URL base = null;
		try {
			base = FileLocator.resolve(XmlEditorPlugin.getDefault().getBundle().getEntry("/")); //$NON-NLS-1$
		} catch (IOException e) {
			XmlEditorPlugin.getPluginLog().logError(e);
			return null;
		}
		StringBuffer buffer = new StringBuffer(prefix);
		buffer.append('/');
		buffer.append(name);
		return new URL(base, buffer.toString());
	}

	public void dispose() {
		if (fPreferenceStore != null) {
			fPreferenceStore.removePropertyChangeListener(this);
			fPreferenceStore = null;
		}
		fOccurrencePreferenceProvider = null;
	}

}
