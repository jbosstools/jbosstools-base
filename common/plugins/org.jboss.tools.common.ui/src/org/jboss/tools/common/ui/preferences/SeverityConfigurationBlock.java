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

import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.corext.util.Messages;
import org.eclipse.jdt.internal.ui.dialogs.StatusInfo;
import org.eclipse.jdt.internal.ui.preferences.OptionsConfigurationBlock;
import org.eclipse.jdt.internal.ui.preferences.PreferencesMessages;
import org.eclipse.jdt.internal.ui.preferences.ScrolledPageContent;
import org.eclipse.jdt.internal.ui.wizards.IStatusChangeListener;
import org.eclipse.jface.dialogs.ControlEnableState;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.layout.PixelConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.preferences.IWorkbenchPreferenceContainer;
import org.jboss.tools.common.preferences.SeverityPreferences;

/**
 * See more info in SeverityPreferences.
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
 *    Also, see ProblemSeveritiesConfigurationBlock
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

	protected PixelConverter fPixelConverter;

	protected FilteredPreferenceTree fFilteredPrefTree;

	private ControlEnableState mainBlockEnableState;

	/**
	 * Text control retrieved from fFilteredPrefTree.
	 */
	protected Text filterControl;

	protected IWorkbenchPreferenceContainer container;

	protected Button enableCheckBox;

	public SeverityConfigurationBlock(IStatusChangeListener context,
			IProject project, Key[] allKeys,
			IWorkbenchPreferenceContainer container) {
		super(context, project,  allKeys, container);
		this.container = container;
	}

	/**
	 * @return the container
	 */
	public IWorkbenchPreferenceContainer getContainer() {
		return container;
	}

	protected abstract String getCommonDescription();

	protected abstract SectionDescription[] getAllSections();

	protected abstract IDialogSettings getDialogSettings();

	protected Composite createStyleTabContent(Composite folder) {
		int nColumns = 3;
		GridLayout layout= new GridLayout(nColumns, false);
		layout.marginHeight= 0;
		layout.marginWidth= 0;
		Composite c = new Composite(folder, 0);
		c.setLayout(layout);
		c.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		if(getMaxNumberOfProblemsKey() != null) {
			addMaxNumberOfMarkersField(c);
		}
		addWrongBuilderOrderField(c);

		Control[] currentControls = folder.getChildren();

		fFilteredPrefTree = new FilteredPreferenceTree(this, folder, getCommonDescription());

		filterControl = findText(folder, currentControls.length);
		
		final ScrolledPageContent sc1 = fFilteredPrefTree.getScrolledPageContent();

		Composite composite = sc1.getBody();

		composite.setLayout(layout);

		int defaultIndent = 0;

		for (SectionDescription section: getAllSections()) {
			createSection(null, section, composite, nColumns, defaultIndent);
		}

		restoreSectionExpansionStates(getDialogSettings());

		return sc1;
	}

	Text findText(Composite composite, int startFromIndex) {
		Control[] cs = composite.getChildren();
		for (int i = startFromIndex; i < cs.length; i++) {
			Control cl = cs[i];
			if(cl instanceof Text) {
				return (Text)cl;
			} else if(cl instanceof Composite) {
				Text t = findText((Composite)cl, 0);
				if(t != null) {
					return t;
				}
			}
		}
		return null;
	}

	protected Composite createInnerComposite(ExpandableComposite excomposite, int nColumns, Font font) {
		Composite inner= new Composite(excomposite, SWT.NONE);
		inner.setFont(font);
		inner.setLayout(new GridLayout(nColumns, false));
		excomposite.setClient(inner);
		return inner;
	}

	protected void createSection(PreferenceTreeNode parent, SectionDescription section, Composite composite, int nColumns, int defaultIndent) {
		String label = section.getLabel(); 

		Key twistieKey = OptionsConfigurationBlock.getLocalKey(label.replace(' ', '_'));
		PreferenceTreeNode treeSection = fFilteredPrefTree.addExpandableComposite(composite, label, nColumns, twistieKey, parent, false);
		ExpandableComposite excomposite = getExpandableComposite(twistieKey);
		Composite inner = createInnerComposite(excomposite, nColumns, composite.getFont());

		for (SectionDescription s: section.getSections()) {
			createSection(treeSection, s, inner, nColumns, defaultIndent);
		}

		for (OptionDescription option: section.getOptions()) {
			label = option.label;
			fFilteredPrefTree.addComboBox(inner, label, option.key, errorWarningIgnore, errorWarningIgnoreLabels, defaultIndent, treeSection);
		}
	}

	@Override
	public void performDefaults() {
		super.performDefaults();
		updateMainPreferenceContent();
	}

	protected Button addEnableField(Composite composite) {
		Button checkBox = addCheckBox(composite, SeverityPreferencesMessages.ENABLE_VALIDATION, getEnableBlockKey(), enableDisableValues, 0);
		checkBox.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				updateMainPreferenceContent();
			}
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		return checkBox;
	}

	protected void addMaxNumberOfMarkersField(Composite composite) {
		Text text = addTextField(composite, SeverityPreferencesMessages.MAX_NUMBER_OF_MARKERS, getMaxNumberOfProblemsKey(), 0, 0);
		GridData gd = (GridData) text.getLayoutData();
		gd.widthHint = fPixelConverter.convertWidthInCharsToPixels(8);
		gd.horizontalAlignment = GridData.BEGINNING;
		text.setLayoutData(gd);
		text.setTextLimit(6);
	}

	protected void addWrongBuilderOrderField(Composite composite) {
		if(getWrongBuilderOrderKey() != null) {
			addComboBox(composite, SeverityPreferencesMessages.WRONG_BUILDER_ORDER, getWrongBuilderOrderKey(), errorWarningIgnore, errorWarningIgnoreLabels, 0);
		}
	}

	private IStatus validateMaxNumberProblems() {
		StatusInfo status= new StatusInfo();
		if(getMaxNumberOfProblemsKey() != null) {
			String number = getValue(getMaxNumberOfProblemsKey());
			if (number == null || number.length() == 0) {
				status.setError(PreferencesMessages.JavaBuildConfigurationBlock_empty_input);
			} else {
				try {
					int value= Integer.parseInt(number);
					if (value <= 0) {
						status.setError(Messages.format(PreferencesMessages.JavaBuildConfigurationBlock_invalid_input, number));
					}
				} catch (NumberFormatException e) {
					status.setError(Messages.format(PreferencesMessages.JavaBuildConfigurationBlock_invalid_input, number));
				}
			}
		}
		return status;
	}

	abstract protected Key getEnableBlockKey();

	abstract protected Key getMaxNumberOfProblemsKey();

	/**
	 * Returns Key object for preference controlling builders order if it is relevant.
	 * If builders order is not relevant, returns null.
	 * @return unique key or null
	 */
	protected Key getWrongBuilderOrderKey() {
		return null;
	}

	private Composite commonComposite;

	@Override
	protected Control createContents(Composite parent) {
		fPixelConverter = new PixelConverter(parent);
		setShell(parent.getShell());

		Composite mainComp = new Composite(parent, SWT.NONE);
		mainComp.setFont(parent.getFont());
		GridLayout layout= new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		mainComp.setLayout(layout);
		mainComp.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		enableCheckBox = addEnableField(mainComp);

		commonComposite = createStyleTabContent(mainComp);
		GridData gridData = new GridData(GridData.FILL, GridData.FILL, true, true);
		gridData.heightHint = convertHeightInCharsToPixels(parent,20);
		commonComposite.setLayoutData(gridData);

		validateSettings(null, null, null);

		updateMainPreferenceContent();

		return mainComp;
	}

	protected void updateMainPreferenceContent() {
		if (enableCheckBox.getSelection()) {
			if (mainBlockEnableState != null) {
				mainBlockEnableState.restore();
				mainBlockEnableState= null;
			}
		} else {
			if (mainBlockEnableState == null) {
				mainBlockEnableState= ControlEnableState.disable(commonComposite);
			}
		}
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

		if(getMaxNumberOfProblemsKey() != null) {
			if (changedKey == null || getMaxNumberOfProblemsKey().equals(changedKey)) {
				fContext.statusChanged(validateMaxNumberProblems());
			}
		}
	}

	public static class SectionDescription {
		private String label;
		private SectionDescription[] sections;
		private OptionDescription[] options;

		public SectionDescription(String label, String[][] optionLabelsAndKeys, String pluginId) {
			this(label, new SectionDescription[0], optionLabelsAndKeys, pluginId);
		}
		public SectionDescription(String label, SectionDescription[] sections, String[][] optionLabelsAndKeys, String pluginId) {
			this.label = label;
			this.sections = sections;
			options = new OptionDescription[optionLabelsAndKeys.length];
			for (int i = 0; i < options.length; i++) {
				options[i] = new OptionDescription(optionLabelsAndKeys[i][0], optionLabelsAndKeys[i][1], pluginId);
			}
		}
		public String getLabel() {
			return label;
		}
		public SectionDescription[] getSections() {
			return sections;
		}
		public OptionDescription[] getOptions() {
			return options;
		}
		public void collectKeys(ArrayList<Key> keys) {
			for (SectionDescription s: sections) {
				s.collectKeys(keys);
			}
			for (OptionDescription o: options) {
				keys.add(o.key);
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

	public Text getFilterControl() {
		return filterControl;
	}
	
	public void doFilter(String prefId){
//		String qualifier = getQualifier();
		Key key = null;
		for(Key k : fAllKeys){
			if(/*k.getQualifier().equals(qualifier) &&*/ k.getName().equals(prefId))
				key = k;
		}
		if(key != null){
			Combo combo = getComboBox(key);
			if(combo != null){
				String value = ((Label)fLabels.get(combo)).getText();
			
				if(value != null) {
					if(filterControl != null) {
						filterControl.setText(value);
					} else {
						fFilteredPrefTree.doFilter(value);
					}
				}
			}
		}
	}
	
	abstract protected String getQualifier();

}