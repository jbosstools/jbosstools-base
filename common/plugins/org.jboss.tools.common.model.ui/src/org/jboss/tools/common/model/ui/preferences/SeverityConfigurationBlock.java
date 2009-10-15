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

package org.jboss.tools.common.model.ui.preferences;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.ui.dialogs.StatusInfo;
import org.eclipse.jdt.internal.ui.preferences.OptionsConfigurationBlock;
import org.eclipse.jdt.internal.ui.wizards.IStatusChangeListener;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.preferences.IWorkbenchPreferenceContainer;
import org.jboss.tools.common.preferences.SeverityPreferences;

/**
 * Find in SeverirtyPreferences the instruction to Framework for Severity preferences
 * To modify section descriptions:
 * 1) If new option is to be added to existing description,
 *    add array of two String objects, where first is the preference name 
 *    defined in SeverityPreferences, and second is label defined in 
 *    SeverityPreferencesMessages (do not forget put property to SeverirtyPreferencesMessages.properties
 *    and constant to SeverityPreferencesMessages.java)
 *    
 * 2) If new section named A is to be created create constant
 *		private static SectionDescription SECTION_A = new SectionDescription(
 *			SeamPreferencesMessages.SeamValidatorConfigurationBlock_section_a,
 *			new String[][]{
 *			}
 *		);
 *    create required constant and property in SeverityPreferencesMessages, 
 *    and add SECTION_A to array ALL_SECTIONS.
 * 
 * @author Viacheslav Kabanovich
 */
abstract public class SeverityConfigurationBlock extends OptionsConfigurationBlock {

	protected static final String ERROR = SeverityPreferences.ERROR;
	protected static final String WARNING = SeverityPreferences.WARNING;
	protected static final String IGNORE = SeverityPreferences.IGNORE;

	protected static final String ENABLED = JavaCore.ENABLED;
	protected static final String DISABLED = JavaCore.DISABLED;

	protected abstract Composite createStyleTabContent(Composite folder);

	public SeverityConfigurationBlock(IStatusChangeListener context,
			IProject project, Key[] allKeys,
			IWorkbenchPreferenceContainer container) {
		super(context, project, allKeys, container);
	}

	@Override
	protected Control createContents(Composite parent) {
		setShell(parent.getShell());

		Composite mainComp = new Composite(parent, SWT.NONE);
		mainComp.setFont(parent.getFont());
		GridLayout layout= new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		mainComp.setLayout(layout);

		Composite commonComposite = createStyleTabContent(mainComp);
		GridData gridData = new GridData(GridData.FILL, GridData.FILL, true, true);
		gridData.heightHint = convertHeightInCharsToPixels(parent,20);
		commonComposite.setLayoutData(gridData);

		validateSettings(null, null, null);

		return mainComp;
	}

	private int convertHeightInCharsToPixels(Control control,int chars) {
		Font font = control.getFont();
		GC gc = new GC(font.getDevice());
		gc.setFont(font);
		FontMetrics fFontMetrics = gc.getFontMetrics();
		gc.dispose();
		return Dialog.convertHeightInCharsToPixels(fFontMetrics, chars);
	}

	@Override
	protected String[] getFullBuildDialogStrings(boolean workspaceSettings) {
		String title= SeverityPreferencesMessages.ValidatorConfigurationBlock_needsbuild_title; 
		String message;
		if (workspaceSettings) {
			message= SeverityPreferencesMessages.ValidatorConfigurationBlock_needsfullbuild_message; 
		} else {
			message= SeverityPreferencesMessages.ValidatorConfigurationBlock_needsprojectbuild_message; 
		}
		return new String[] { title, message };
	}

	@Override
	protected void validateSettings(Key changedKey, String oldValue,
			String newValue) {
		if (!areSettingsEnabled()) {
			return;
		}

		fContext.statusChanged(new StatusInfo());		
	}

	public static class SectionDescription {
		public String label;
		public OptionDescription[] options;

		public SectionDescription(String label, String[][] optionLabelsAndKeys, String pluginId) {
			this.label = label;
			options = new OptionDescription[optionLabelsAndKeys.length];
			for (int i = 0; i < options.length; i++) {
				options[i] = new OptionDescription(optionLabelsAndKeys[i][0], optionLabelsAndKeys[i][1], pluginId);
			}
		}
	}

	public static class OptionDescription {
		public String label;
		public Key key;

		public OptionDescription(String keyName, String label, String pluginId) {
			this.label = label;
			key = getKey(pluginId, keyName);
		}
	}
}