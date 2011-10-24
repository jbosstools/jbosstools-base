 /*******************************************************************************
  * Copyright (c) 2007-2009 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.types;

import static org.junit.Assert.fail;

import java.util.LinkedList;
import java.util.List;

/**
 * Provides functionality for evaluation particular entity location like projects types, file types, etc.
 * @author jpeterka
 */
public enum EntityType {
	HIBERNATE_MAPPING_FILE, JAVA_PROJECT, JAVA_CLASS, JAVA_PACKAGE, HIBERNATE_REVERSE_FILE, HIBERNATE_CONSOLE, 
	HIBERNATE_CONFIGURATION_FILE, STRUTS_PROJECT, JPA_PROJECT, DROOLS_PROJECT, DROOLS_RULE,
	GUIDED_DROOLS_RULE,DSL_DROOLS_FILE,RESOURCES_FROM_GUVNOR,SMOOKS_CONFIG, JBPM3_PROJECT;
		
	public List<String> getGroupsLabels() {
	  List<String> groupLabel = new LinkedList<String>();
		
		switch (this) {
		case HIBERNATE_MAPPING_FILE: groupLabel.add(IDELabel.EntityGroup.HIBERNATE); break;
		case JAVA_PROJECT: groupLabel.add(IDELabel.EntityGroup.JAVA); break;
		case JAVA_CLASS: groupLabel.add(IDELabel.EntityGroup.JAVA); break;	
		case JAVA_PACKAGE: groupLabel.add(IDELabel.EntityGroup.JAVA); break;
		case HIBERNATE_REVERSE_FILE: groupLabel.add(IDELabel.EntityGroup.HIBERNATE); break;		
		case HIBERNATE_CONFIGURATION_FILE: groupLabel.add(IDELabel.EntityGroup.HIBERNATE); break;
		case HIBERNATE_CONSOLE: groupLabel.add(IDELabel.EntityGroup.HIBERNATE); break;
		case STRUTS_PROJECT: groupLabel.add(IDELabel.EntityGroup.JBOSS_TOOLS_WEB); groupLabel.add(IDELabel.EntityGroup.STRUTS);break;
		case JPA_PROJECT: groupLabel.add(IDELabel.EntityGroup.JPA);break;
		case DROOLS_PROJECT: groupLabel.add(IDELabel.EntityGroup.DROOLS);break;
		case DROOLS_RULE: groupLabel.add(IDELabel.EntityGroup.DROOLS);break;
		case GUIDED_DROOLS_RULE: groupLabel.add(IDELabel.EntityGroup.DROOLS);break;
		case DSL_DROOLS_FILE: groupLabel.add(IDELabel.EntityGroup.DROOLS);break;
		case RESOURCES_FROM_GUVNOR: groupLabel.add(IDELabel.EntityGroup.GUVNOR);break;
		case SMOOKS_CONFIG: groupLabel.add(IDELabel.EntityGroup.SMOOKS);break;
		case JBPM3_PROJECT: groupLabel.add(IDELabel.EntityGroup.JBPM);break;
		default: fail("Unknown Entity Type");
		}
		
		return groupLabel;
	}
	
	/**
	 * Return entity label
	 * @return
	 */
	public String getEntityLabel() {
		String entityLabel = "";
		
		switch (this) {
		case HIBERNATE_MAPPING_FILE: entityLabel = IDELabel.EntityLabel.HIBERNATE_MAPPING_FILE; break;
		case HIBERNATE_REVERSE_FILE: entityLabel = IDELabel.EntityLabel.HIBERNATE_REVERSE_FILE; break;
		case HIBERNATE_CONFIGURATION_FILE: entityLabel = IDELabel.EntityLabel.HIBERNATE_CONFIGURATION_FILE; break;
		case HIBERNATE_CONSOLE: entityLabel = IDELabel.EntityLabel.HIBERNATE_CONSOLE; break;
		case JAVA_PROJECT: entityLabel = IDELabel.EntityLabel.JAVA_PROJECT; break;
		case JAVA_CLASS: entityLabel = IDELabel.EntityLabel.JAVA_CLASS; break;
		case JAVA_PACKAGE: entityLabel = IDELabel.EntityLabel.JAVA_PACKAGE; break;
		case STRUTS_PROJECT: entityLabel = IDELabel.EntityLabel.STRUTS_PROJECT; break;
		case JPA_PROJECT: entityLabel = IDELabel.EntityLabel.JPA_PROJECT; break;
		case DROOLS_PROJECT: entityLabel = IDELabel.EntityLabel.DROOLS_PROJECT; break;
		case DROOLS_RULE:  entityLabel = IDELabel.EntityLabel.DROOLS_RULE; break;
		case GUIDED_DROOLS_RULE:  entityLabel = IDELabel.EntityLabel.GUIDED_DROOLS_RULE; break;
		case DSL_DROOLS_FILE:  entityLabel = IDELabel.EntityLabel.DSL_DROOLS_FILE; break;		
		case RESOURCES_FROM_GUVNOR:  entityLabel = IDELabel.EntityLabel.RESOURCES_FROM_GUVNOR; break;
		case SMOOKS_CONFIG: entityLabel = IDELabel.EntityLabel.SMOOKS_CONF_FILE; break;
		case JBPM3_PROJECT: entityLabel = IDELabel.EntityLabel.JBPM3_PROJECT; break;
		default: fail("Unknown Entity Type");
		}		
		
		return entityLabel;
	}
}