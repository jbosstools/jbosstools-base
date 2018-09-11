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

import org.eclipse.core.runtime.IPath;

import java.util.Collection;
import java.util.Collections;
import java.util.stream.Collectors;

import org.eclipse.core.resources.ResourcesPlugin;
import org.jboss.tools.common.databinding.ObservablePojo;
import org.jboss.tools.common.launcher.core.model.Booster;
import org.jboss.tools.common.launcher.core.model.Catalog;
import org.jboss.tools.common.launcher.core.model.Mission;

public class NewLauncherProjectModel extends ObservablePojo {
	
	public static final String PROJECT_NAME_PROPERTY = "projectName";
	public static final String USE_DEFAULT_LOCATION_PROPERTY = "useDefaultLocation";
	public static final String LOCATION_PROPERTY = "location";
	public static final String GROUPID_PROPERTY = "groupId";
	public static final String ARTIFACTID_PROPERTY = "artifactId";
	public static final String VERSION_PROPERTY = "version";
	public static final String MISSIONS_PROPERTY = "missions";
	public static final String SELECTED_MISSION_PROPERTY = "selectedMission";
	public static final String BOOSTERS_PROPERTY = "boosters";
	public static final String SELECTED_BOOSTER_PROPERTY = "selectedBooster";

	private static final IPath ROOT = ResourcesPlugin.getWorkspace().getRoot().getLocation();

	private String projectName;
	private boolean useDefaultLocation = true;
	private IPath location = ROOT;
	private String groupId = "io.openshift";
	private String artifactId = "booster";
	private String version = "0.0.1-SNAPSHOT";
	private Catalog catalog;
	private Collection<Mission> missions = Collections.emptyList();
	private Mission selectedMission;
	private Collection<Booster> boosters = Collections.emptyList();
	private Booster selectedBooster;
	
	/**
	 * @return the name of the project
	 */
	public String getProjectName() {
		return projectName;
	}

	/**
	 * @param projectName the new name of the project
	 */
	public void setProjectName(String projectName) {
		firePropertyChange(PROJECT_NAME_PROPERTY, this.projectName, this.projectName = projectName);
		updateLocation();
	}

	/**
	 * updates the location for the existing project name.
	 * 
	 * @see #getProjectName()
	 */
	private void updateLocation() {
		if (isUseDefaultLocation()) {
			setLocation(ROOT.append(getProjectName()));
		}
	}

	/**
	 * @return {@code true} if the model is set to use the default location. Returns
	 *         {@code false} otherwise.
	 */
	public boolean isUseDefaultLocation() {
		return useDefaultLocation;
	}

	/**
	 * sets this model to use / or not use the default location for the generated
	 * project.
	 * 
	 * @param useDefaultLocation whether to use the default location to generate the
	 *                           project to.
	 */
	public void setUseDefaultLocation(boolean useDefaultLocation) {
		firePropertyChange(USE_DEFAULT_LOCATION_PROPERTY, this.useDefaultLocation, this.useDefaultLocation = useDefaultLocation);
		updateLocation();
	}

	/**
	 * @return the location that the project is generated to
	 */
	public IPath getLocation() {
		return location;
	}

	/**
	 * @param location the new location to create the project to.
	 */
	public void setLocation(IPath location) {
		firePropertyChange(LOCATION_PROPERTY, this.location, this.location = location);
	}

	/**
	 * @return the maven groupId for the project that will be created.
	 */
	public String getGroupId() {
		return groupId;
	}

	/**
	 * @param groupId the new groupId for the project that this model will create.
	 */
	public void setGroupId(String groupId) {
		firePropertyChange(GROUPID_PROPERTY, this.groupId, this.groupId = groupId);
	}

	/**
	 * @return the maven artifactId for the project that will be created.
	 */
	public String getArtifactId() {
		return artifactId;
	}

	/**
	 * @param artifactId the new artifactId for the project that will be created.
	 */
	public void setArtifactId(String artifactId) {
		firePropertyChange(ARTIFACTID_PROPERTY, this.artifactId, this.artifactId = artifactId);
	}

	/**
	 * @return the version of the maven artifact that this model will create.
	 */
	public String getVersion() {
		return version;
	}

	/**
	 * @param version the new version for the project that this model will create
	 */
	public void setVersion(String version) {
		firePropertyChange(VERSION_PROPERTY, this.version, this.version = version);
	}

	/**
	 * @param catalog the catalog that the mission can be chosen from.
	 */
	public void setCatalog(Catalog catalog) {
		this.catalog = catalog;
		firePropertyChange(MISSIONS_PROPERTY, this.missions, this.missions = catalog.getMissions());
		if (!this.missions.isEmpty()) {
			setSelectedMission(this.missions.iterator().next());
		}
	}

	/**
	 * @return the missions that are available to choose from.
	 */
	public Collection<Mission> getMissions() {
		return missions;
	}

	/**
	 * @return the selectedMission the mission that is selected among the available ones.
	 */
	public Mission getSelectedMission() {
		return selectedMission;
	}

	/**
	 * @param selectedMission the mission that should be selected.
	 */
	public void setSelectedMission(Mission selectedMission) {
		firePropertyChange(SELECTED_MISSION_PROPERTY, this.selectedMission, this.selectedMission = selectedMission);
		setBoosters(catalog.getBoosters().stream().filter(booster -> booster.getMission().equals(selectedMission.getId())).collect(Collectors.toList()));
	}

	/**
	 * @return the boosters (example applications) that are available in the selected mission.
	 * 
	 * @see #getSelectedMission()
	 */
	public Collection<Booster> getBoosters() {
		return boosters;
	}

	/**
	 * @param boosters sets the boosters (example applications) that are available for the selected mission.
	 */
	public void setBoosters(Collection<Booster> boosters) {
		firePropertyChange(BOOSTERS_PROPERTY, this.boosters, this.boosters = boosters);
		if (!this.boosters.isEmpty()) {
			setSelectedBooster(this.boosters.iterator().next());
		}
	}

	/**
	 * @return the booster that is selected among the available ones.
	 * 
	 * @see #getMissions()
	 */
	public Booster getSelectedBooster() {
		return selectedBooster;
	}

	/**
	 * @param selectedBooster sets the new selected booster
	 */
	public void setSelectedBooster(Booster selectedBooster) {
		firePropertyChange(SELECTED_BOOSTER_PROPERTY, this.selectedBooster, this.selectedBooster = selectedBooster);
	}
	
	
	
	
}
