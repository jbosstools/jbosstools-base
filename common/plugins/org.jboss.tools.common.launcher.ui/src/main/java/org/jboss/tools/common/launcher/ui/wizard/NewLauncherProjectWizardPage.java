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
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
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

	private NewLauncherProjectModel model;

	public NewLauncherProjectWizardPage(IWizard wizard, NewLauncherProjectModel model) {
		super("Generate a project based on mission and runtime.", 
				"A mission is a specification that describes what your application will do. "
				+ "A runtime is the framework software used in the application's process.", 
				"main", wizard, null);
		this.model = model;
	}

	@Override
	protected void doCreateControls(Composite parent, DataBindingContext dbc) {
		GridDataFactory.fillDefaults()
			.grab(true, false).align(SWT.FILL, SWT.TOP)
			.applyTo(parent);
		GridLayoutFactory.fillDefaults().margins(6, 6).numColumns(2).applyTo(parent);

		// project name
		createTextWidget(parent, dbc, "Project name", NewLauncherProjectModel.PROJECT_NAME_PROPERTY, new EclipseProjectValidator("Please specify an Eclipse project", "Project already exists"));

		//use default location
		Button buttonUseDefaultLocation = new Button(parent, SWT.CHECK);
		buttonUseDefaultLocation.setText("Use default location");
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER).span(2, 1)
			.applyTo(buttonUseDefaultLocation);
		IObservableValue<Boolean> useDefaultLocationButtonObservable = WidgetProperties.selection().observe(buttonUseDefaultLocation);
		ValueBindingBuilder.bind(useDefaultLocationButtonObservable)
				.to(BeanProperties.value(NewLauncherProjectModel.USE_DEFAULT_LOCATION_PROPERTY).observe(model))
				.in(dbc);

		// location
		Label lblLocation = new Label(parent, SWT.NONE);
		lblLocation.setText("Location");
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER)
			.applyTo(lblLocation);

		Text txtLocation = new Text(parent, SWT.BORDER);
		GridDataFactory.fillDefaults()
			.align(SWT.FILL, SWT.CENTER).grab(true, false)
			.applyTo(txtLocation);
		ValueBindingBuilder.bind(WidgetProperties.text(SWT.Modify).observe(txtLocation))
				.validatingAfterGet(new MandatoryStringValidator("Please specify a location for you project"))
				.converting(IConverter.create(String.class, IPath.class, NewLauncherProjectWizardPage::string2IPath))
				.to(BeanProperties.value(NewLauncherProjectModel.LOCATION_PROPERTY).observe(model)).in(dbc);
		ValueBindingBuilder.bind(WidgetProperties.enabled().observe(txtLocation))
				.notUpdatingParticipant()
				.to(BeanProperties.value(NewLauncherProjectModel.USE_DEFAULT_LOCATION_PROPERTY).observe(model))
				.converting(new InvertingBooleanConverter()).in(dbc);

		// missions
		Label lblMissions = new Label(parent, SWT.NONE);
		lblMissions.setText("Select the mission");
		lblMissions.setToolTipText("A specification that describes what your application will do.");
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER)
			.applyTo(lblMissions);
		Combo comboMissions = new Combo(parent, SWT.SINGLE | SWT.DROP_DOWN | SWT.READ_ONLY);
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER)
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
		ValueBindingBuilder.bind(ViewerProperties.singleSelection().observe(comboMissionsViewer))
			.to(BeanProperties.value(NewLauncherProjectModel.SELECTED_MISSION_PROPERTY).observe(model))
			.in(dbc);

		//boosters
		Label lblBoosters = new Label(parent, SWT.NONE);
		lblBoosters.setText("Select the runtime");
		lblBoosters.setToolTipText("The framework software used in the application's process.");
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER)
			.applyTo(lblBoosters);
		Combo comboBoosters = new Combo(parent, SWT.SINGLE | SWT.DROP_DOWN | SWT.READ_ONLY);
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER).hint(200, SWT.DEFAULT)
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
		ValueBindingBuilder.bind(ViewerProperties.singleSelection().observe(comboBoostersViewer))
			.to(BeanProperties.value(NewLauncherProjectModel.SELECTED_BOOSTER_PROPERTY).observe(model))
			.in(dbc);
		
		createTextWidget(parent, dbc, "Artifact id", NewLauncherProjectModel.ARTIFACTID_PROPERTY, new MandatoryStringValidator("Please specify an artifact id"));
		createTextWidget(parent, dbc, "Group id", NewLauncherProjectModel.GROUPID_PROPERTY, new MandatoryStringValidator("Please specify a group id"));
		createTextWidget(parent, dbc, "Version", NewLauncherProjectModel.VERSION_PROPERTY, new MandatoryStringValidator("Please specify a version"));
		
		loadCatalog();
	}
	
	private Widget createTextWidget(Composite parent, DataBindingContext dbc, String label, String property, IValidator<String> validator) {
		// artifact id
		Label lbl = new Label(parent, SWT.NONE);
		lbl.setText(label);
		GridDataFactory.fillDefaults()
			.align(SWT.LEFT, SWT.CENTER)
			.applyTo(lbl);

		Text text = new Text(parent, SWT.BORDER);
		GridDataFactory.fillDefaults()
			.align(SWT.FILL, SWT.CENTER).grab(true, false)
			.applyTo(text);
		Binding binding = ValueBindingBuilder.bind( WidgetProperties.text(SWT.Modify).observe(text))
				.validatingAfterConvert(validator)
				.to(BeanProperties.value(property).observe(model)).in(dbc);
		ControlDecorationSupport.create(binding, SWT.LEFT | SWT.TOP, null, new RequiredControlDecorationUpdater());
		return text;
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

}
