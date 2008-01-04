/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.kb;

import java.util.Collection;

/**
 * Describes Dinamic Resorce.
 * This resource provides any information from project model.
 * @author igels
 */
public interface KbDinamicResource extends KbResource {

	String BUNDLE_PROPERTY_TYPE = "bundleProperty";
	String BEAN_PROPERTY_TYPE = "beanProperty";
	String BEAN_METHOD_BY_SYGNATURE_TYPE = "beanMethodBySignature";
	String JSP_PATH_TYPE = "jspPath";
	String BUNDLE_NAME_TYPE = "bundleName";
	String VIEW_ACTIONS_TYPE = "viewActions";
	String IMAGE_FILE_TYPE = "file";
	String ENUMERATION_TYPE = "enumeration";
	String JSF_VARIABLES_TYPE = "jsfVariables";
	String FACELETS_JSFC_TYPE = "faceletsJsfCTags";
	String MANAGED_BEAN_NAME_TYPE = "managedBeanName";
	String JSF_ID = "jsfID";
	String TAGLIB_TYPE = "taglib";

	/**
	 * Sets a parameter for resource.
	 * @param name
	 * @param value
	 */
	public void setConstraint(String name, String value);

	/**
	 * Clear every parameter.
	 */
	public void clearConstraints();

	/**
	 * @param query
	 * @return Collection of proposals.
	 */
	public Collection<KbProposal> queryProposal(String query);

	/**
	 * @return Type of Resource.
	 */
	public String getType();
}