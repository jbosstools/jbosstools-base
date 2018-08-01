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
package org.jboss.tools.common.launcher.core.model;

import java.util.Collection;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown=true)
public class Catalog {
	@JsonProperty
	private Collection<Booster> boosters;
	
	@JsonProperty
	private Collection<Runtime> runtimes;
	
	@JsonProperty
	private Collection<Mission> missions;

	/**
	 * @return the boosters
	 */
	public Collection<Booster> getBoosters() {
		return boosters;
	}

	/**
	 * @param boosters the boosters to set
	 */
	public void setBoosters(Collection<Booster> boosters) {
		this.boosters = boosters;
	}

	/**
	 * @return the runtimes
	 */
	public Collection<Runtime> getRuntimes() {
		return runtimes;
	}

	/**
	 * @param runtimes the runtimes to set
	 */
	public void setRuntimes(Collection<Runtime> runtimes) {
		this.runtimes = runtimes;
	}

	/**
	 * @return the missions
	 */
	public Collection<Mission> getMissions() {
		return missions;
	}

	/**
	 * @param missions the missions to set
	 */
	public void setMissions(Collection<Mission> missions) {
		this.missions = missions;
	}

}
