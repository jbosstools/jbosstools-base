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

package org.jboss.tools.common.ui.preferences;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.ui.dialogs.StatusInfo;
import org.eclipse.jdt.internal.ui.preferences.OptionsConfigurationBlock;
import org.eclipse.jdt.internal.ui.preferences.ScrolledPageContent;
import org.eclipse.jdt.internal.ui.wizards.IStatusChangeListener;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
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

	protected String[] errorWarningIgnore = new String[] {ERROR, WARNING, IGNORE};
	protected String[] enableDisableValues= new String[] {ENABLED, DISABLED};

	protected String[] errorWarningIgnoreLabels = new String[] {
		SeverityPreferencesMessages.VALIDATOR_CONFIGURATION_BLOCK_ERROR,  
		SeverityPreferencesMessages.VALIDATOR_CONFIGURATION_BLOCK_WARNING, 
		SeverityPreferencesMessages.VALIDATOR_CONFIGURATION_BLOCK_IGNORE
	};

	public SeverityConfigurationBlock(IStatusChangeListener context,
			IProject project, Key[] allKeys,
			IWorkbenchPreferenceContainer container) {
		super(context, project, allKeys, container);
	}

	protected abstract String getCommonDescription();

	protected abstract SectionDescription[] getAllSections();

	protected abstract IDialogSettings getDialogSettings();

	protected Composite createStyleTabContent(Composite folder) {
		int nColumns = 3;

		final ScrolledPageContent sc1 = new ScrolledPageContent(folder);

		Composite composite = sc1.getBody();
		GridLayout layout= new GridLayout(nColumns, false);
		layout.marginHeight= 0;
		layout.marginWidth= 0;
		composite.setLayout(layout);

		Label description= new Label(composite, SWT.LEFT | SWT.WRAP);
		description.setFont(description.getFont());
		description.setText(getCommonDescription()); 
		description.setLayoutData(new GridData(GridData.BEGINNING, GridData.CENTER, true, false, nColumns - 1, 1));

		int defaultIndent = 0;

		SectionDescription[] sections = getAllSections();
		for (int i = 0; i < sections.length; i++) {
			SectionDescription section = sections[i];
			String label = section.label; 
			ExpandableComposite excomposite = createStyleSection(composite, label, nColumns);

			Composite inner = new Composite(excomposite, SWT.NONE);
			inner.setFont(composite.getFont());
			inner.setLayout(new GridLayout(nColumns, false));
			excomposite.setClient(inner);

			for (int j = 0; j < section.options.length; j++) {
				OptionDescription option = section.options[j];
				label = option.label;
				addComboBox(inner, label, option.key, errorWarningIgnore, errorWarningIgnoreLabels, defaultIndent);
			}
		}

		IDialogSettings section = getDialogSettings();
		restoreSectionExpansionStates(section);

		return sc1;
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