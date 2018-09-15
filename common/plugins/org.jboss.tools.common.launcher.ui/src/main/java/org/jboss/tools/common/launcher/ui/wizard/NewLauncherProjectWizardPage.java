/*************************************************************************************
 * Copyright (c) 2018 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.launcher.ui.wizard;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.databinding.Binding;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.beans.PojoProperties;
import org.eclipse.core.databinding.conversion.IConverter;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.databinding.fieldassist.ControlDecorationSupport;
import org.eclipse.jface.databinding.swt.WidgetProperties;
import org.eclipse.jface.databinding.viewers.ObservableListContentProvider;
import org.eclipse.jface.databinding.viewers.ViewerProperties;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.common.launcher.core.model.Booster;
import org.jboss.tools.common.launcher.core.model.CatalogManager;
import org.jboss.tools.common.launcher.core.model.Mission;
import org.jboss.tools.common.ui.WizardUtils;
import org.jboss.tools.common.ui.databinding.EclipseProjectValidator;
import org.jboss.tools.common.ui.databinding.InvertingBooleanConverter;
import org.jboss.tools.common.ui.databinding.MandatoryStringValidator;
import org.jboss.tools.common.ui.databinding.RequiredControlDecorationUpdater;
import org.jboss.tools.common.ui.databinding.ValueBindingBuilder;
import org.jboss.tools.common.ui.wizard.AbstractDataBindingWizardPage;

public class NewLauncherProjectWizardPage extends AbstractDataBindingWizardPage {

	private static final ImageDescriptor FABRIC8_LOGO = 
			ImageDescriptor.createFromFile(NewLauncherProjectWizardPage.class, "/icons/fabric8_icon_86px.png");

	private NewLauncherProjectModel model;

	public NewLauncherProjectWizardPage(IWizard wizard, NewLauncherProjectModel model) {
		super("Generate a project based on mission and runtime.",
				"Generate an Eclipse project by specifying a mission and runtime variant.", 
				"main", wizard, null);
		this.model = model;
		setImageDescriptor(FABRIC8_LOGO);
	}

	@Override
	protected void doCreateControls(final Composite parent, DataBindingContext dbc) {
		GridLayoutFactory.fillDefaults()
			.margins(6, 6).numColumns(2)
			.applyTo(parent);

		//  explanation
		Label explanation = new Label(parent, SWT.WRAP);
		explanation.setText("Launcher will generate an application for you."
				+ " By picking a mission you determine what this application will do."
				+ " The runtime then picks the software stack that's used to implement this aim.");
		GridDataFactory.fillDefaults()
			.span(2, 1).align(SWT.FILL, SWT.FILL).grab(true, false)
			.applyTo(explanation);

		// missions
		Label lblMissions = new Label(parent, SWT.NONE);
		lblMissions.setText("Mission:");
		lblMissions.setToolTipText("A specification that describes what your application will do.");
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER)
			.applyTo(lblMissions);
		Combo comboMissions = new Combo(parent, SWT.SINGLE | SWT.DROP_DOWN | SWT.READ_ONLY);
		GridDataFactory.fillDefaults()
			.indent(0,  10).align(SWT.LEFT, SWT.CENTER).hint(300, SWT.DEFAULT)
			.applyTo(comboMissions);
		ComboViewer comboMissionsViewer = new ComboViewer(comboMissions);
		comboMissionsViewer.setContentProvider(new ObservableListContentProvider());
		comboMissionsViewer.setInput(BeanProperties.list(NewLauncherProjectModel.MISSIONS_PROPERTY).observe(model));
		comboMissionsViewer.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(Object element) {
				Mission mission = (Mission) element;
				return mission.getId();
			}
		});
		IObservableValue selectedMissionObservable = 
				BeanProperties.value(NewLauncherProjectModel.SELECTED_MISSION_PROPERTY).observe(model);
		ValueBindingBuilder.bind(ViewerProperties.singleSelection().observe(comboMissionsViewer))
			.to(selectedMissionObservable)
			.in(dbc);

		new Label(parent, SWT.None); // filler
		StyledText missionDescription = createStyledText(parent);
		IObservableValue missionDescriptionObservable = PojoProperties.value(NewLauncherProjectModel.DESCRIPTION_PROPERTY)
				.observeDetail(selectedMissionObservable);
		ValueBindingBuilder
			.bind(WidgetProperties.text().observe(missionDescription))
			.notUpdatingParticipant()
			.to(missionDescriptionObservable)
			.in(dbc);
		missionDescriptionObservable.addValueChangeListener(event -> setToPreferredVerticalSize(getShell()));
		
		// boosters
		Label lblBoosters = new Label(parent, SWT.NONE);
		lblBoosters.setText("Runtime:");
		lblBoosters.setToolTipText("The framework software used in the application's process.");
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER)
			.applyTo(lblBoosters);
		Combo comboBoosters = new Combo(parent, SWT.SINGLE | SWT.DROP_DOWN | SWT.READ_ONLY);
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER).hint(300, SWT.DEFAULT)
			.applyTo(comboBoosters);
		ComboViewer comboBoostersViewer = new ComboViewer(comboBoosters);
		comboBoostersViewer.setContentProvider(new ObservableListContentProvider());
		comboBoostersViewer.setInput(BeanProperties.list(NewLauncherProjectModel.BOOSTERS_PROPERTY).observe(model));
		comboBoostersViewer.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(Object element) {
				Booster booster = (Booster) element;
				return booster.getRuntime() + " " + booster.getVersion();
			}
		});
		IObservableValue<Booster> selectedBoosterObservable = 
				BeanProperties.value(NewLauncherProjectModel.SELECTED_BOOSTER_PROPERTY).observe(model);
		ValueBindingBuilder.bind(ViewerProperties.singleSelection().observe(comboBoostersViewer))
			.to(selectedBoosterObservable)
			.in(dbc);

		new Label(parent, SWT.None); // filler
		StyledText boosterDescription = createStyledText(parent);
		IObservableValue boosterDescriptionObservable = 
				PojoProperties.value(NewLauncherProjectModel.DESCRIPTION_PROPERTY).observeDetail(selectedBoosterObservable);
		ValueBindingBuilder
			.bind(WidgetProperties.text().observe(boosterDescription))
			.notUpdatingParticipant()
			.to(boosterDescriptionObservable)
			.in(dbc);
		boosterDescriptionObservable.addValueChangeListener(event -> setToPreferredVerticalSize(getShell()));

		// separator
		Label mavenSeparator = new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL);
		GridDataFactory.fillDefaults()
			.span(2,1).align(SWT.FILL, SWT.CENTER)
			.applyTo(mavenSeparator);

		// project name
		createTextWidget(parent, dbc, "Project name:", NewLauncherProjectModel.PROJECT_NAME_PROPERTY,
				new EclipseProjectValidator("Please specify an Eclipse project", "Project already exists"));
		//use default location
		Button buttonUseDefaultLocation = new Button(parent, SWT.CHECK);
		buttonUseDefaultLocation.setText("Use default location");
		GridDataFactory.fillDefaults()
			.span(2, 1).align(SWT.LEFT, SWT.CENTER)
			.applyTo(buttonUseDefaultLocation);
		IObservableValue<Boolean> useDefaultLocationButtonObservable = WidgetProperties.selection().observe(buttonUseDefaultLocation);
		ValueBindingBuilder.bind(useDefaultLocationButtonObservable)
				.to(BeanProperties.value(NewLauncherProjectModel.USE_DEFAULT_LOCATION_PROPERTY).observe(model))
				.in(dbc);

		// location
		Label lblLocation = new Label(parent, SWT.NONE);
		lblLocation.setText("Location:");
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER)
			.applyTo(lblLocation);

		Text txtLocation = new Text(parent, SWT.BORDER);
		GridDataFactory.fillDefaults()
			.align(SWT.FILL, SWT.CENTER).grab(true, false)
			.applyTo(txtLocation);
		Binding locationBinding = ValueBindingBuilder.bind(WidgetProperties.text(SWT.Modify).observe(txtLocation))
				.validatingAfterGet(new MandatoryStringValidator("Please specify a location for you project"))
				.converting(IConverter.create(String.class, IPath.class, NewLauncherProjectWizardPage::string2IPath))
				.to(BeanProperties.value(NewLauncherProjectModel.LOCATION_PROPERTY).observe(model)).in(dbc);
		ValueBindingBuilder.bind(WidgetProperties.enabled().observe(txtLocation))
				.notUpdatingParticipant()
				.to(BeanProperties.value(NewLauncherProjectModel.USE_DEFAULT_LOCATION_PROPERTY).observe(model))
				.converting(new InvertingBooleanConverter()).in(dbc);
		ControlDecorationSupport.create(locationBinding, SWT.LEFT | SWT.TOP, null, new RequiredControlDecorationUpdater());

		// separator
		Label launcherSeparator = new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL);
		GridDataFactory.fillDefaults()
			.indent(0, 10).span(2,1).align(SWT.FILL, SWT.CENTER)
			.applyTo(launcherSeparator);

		// maven artifact
		Label mavenArtifactExplanation = new Label(parent, SWT.None);
		mavenArtifactExplanation.setText("Maven Artifact:");
		GridDataFactory.fillDefaults()
			.span(2,1).align(SWT.FILL, SWT.CENTER)
			.applyTo(mavenArtifactExplanation);
		createTextWidget(parent, dbc, "Artifact id:", 
				NewLauncherProjectModel.ARTIFACTID_PROPERTY, new MandatoryStringValidator("Please specify an artifact id"));
		createTextWidget(parent, dbc, "Group id:", 
				NewLauncherProjectModel.GROUPID_PROPERTY, new MandatoryStringValidator("Please specify a group id"));
		createTextWidget(parent, dbc, "Version:", 
				NewLauncherProjectModel.VERSION_PROPERTY, new MandatoryStringValidator("Please specify a version"));
		
		loadCatalog();
	}

	private Text createTextWidget(Composite parent, DataBindingContext dbc, String label, String property, IValidator<String> validator) {
		Label lbl = new Label(parent, SWT.NONE);
		lbl.setText(label);
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER)
			.applyTo(lbl);

		Text text = new Text(parent, SWT.BORDER);
		GridDataFactory.fillDefaults()
			.align(SWT.FILL, SWT.CENTER).grab(true, false)
			.applyTo(text);
		Binding binding = ValueBindingBuilder.bind(WidgetProperties.text(SWT.Modify).observe(text))
				.validatingAfterConvert(validator)
				.to(BeanProperties.value(property).observe(model)).in(dbc);
		ControlDecorationSupport.create(binding, SWT.LEFT | SWT.TOP, null, new RequiredControlDecorationUpdater());
		return text;
	}

	private StyledText createStyledText(Composite parent) {
		StyledText styledText = new StyledText(parent, SWT.WRAP | SWT.READ_ONLY | SWT.V_SCROLL);
		styledText.setAlwaysShowScrollBars(false);
		Display display = styledText.getDisplay();
		styledText.setBackground(display.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
		styledText.setForeground(display.getSystemColor(SWT.COLOR_DARK_GRAY));
		GridDataFactory.fillDefaults()
			.align(SWT.FILL, SWT.FILL).grab(true, false) //.hint(SWT.DEFAULT, 30)
			.applyTo(styledText);
		return styledText; 
	}

	private void loadCatalog() {
		try {
			WizardUtils.runInWizard(Job.create("Loading launcher catalog", 
					monitor -> model.setCatalog(CatalogManager.getDefault().getCatalog(monitor))), getContainer());
		} catch (InvocationTargetException | InterruptedException e) {
			// ignore
		}
	}

	private static IPath string2IPath(String str) {
		return Path.fromOSString(str);
	}

	@Override
	protected void onPageActivated(DataBindingContext dbc) {
		setToPreferredVerticalSize(getShell());
	}

	private void setToPreferredVerticalSize(Shell shell) {
		Point size = shell.computeSize(600, SWT.DEFAULT);
		// windows doesn't take progress bar area into account, maven section gets cut off.
		size.y += 20;
		shell.setSize(size );
	}
}
