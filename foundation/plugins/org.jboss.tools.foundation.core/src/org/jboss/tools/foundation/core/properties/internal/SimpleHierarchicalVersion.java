/******************************************************************************* 
 * Copyright (c) 2014 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.core.properties.internal;

import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 *  <i>Simple</i> Hierarchical version model.  
 */
public class SimpleHierarchicalVersion {
	
	private String internalVersion;
	
	public SimpleHierarchicalVersion(String version) {
		internalVersion = version;
	}

	private static final Pattern MILESTONES_PATTERN = Pattern.compile("^(Alpha|Beta|AM|RC|CR)(\\d+)$");

	/**
	 * Returns a parent {@link SimpleHierarchicalVersion} for this instance, corresponding to one level up in the version hierarchy. 
	 * For instance, it'll return :
	 * <ul>
	 * <li>1.2.3.GA-123456 for 1.2.3.GA-123456-23455</li>
	 * <li>1.2.3.GA for 1.2.3.GA-123456</li>
	 * <li>1.2.3.AM for 1.2.3.AM2 (also works for Alpha*, Beta*, CR*, RC*)</li>
	 * <li>1.2.3 for 1.2.3.GA</li>
	 * <li>1.2 for 1.2.3</li>
	 * <li>1 for 1.2</li>
	 * <li><code>null</code> for 1</li>	 
	 * </ul>
	 * 
	 * @return the parent {@link SimpleHierarchicalVersion} for this instance, or <code>null</code> if it has no parent. 
	 */
	public SimpleHierarchicalVersion getParentVersion() {
		
		String parentVersion = getOneVersionUp(internalVersion);
		if (parentVersion == null || internalVersion.equals(parentVersion)) {
			return null;
		}
		return new SimpleHierarchicalVersion(parentVersion);
	}
	
	static String getOneVersionUp(String version) {
		if (version == null) {
			 return null;
		}
		if (version.endsWith("-SNAPSHOT")) {
			return version.substring(0, version.lastIndexOf("-SNAPSHOT"));
		}
		
		String[] segments = version.split("\\.");
		int newLength = 0;
		int curLength = segments.length;
		
		if (curLength > 3) {
			String qualifier = segments[3];
			int dashIndex = qualifier.lastIndexOf('-');
			newLength = 4;
			if ( dashIndex > 0) {
			  segments[3] = qualifier.substring(0, dashIndex);
			} else {
				if (curLength == 4){
					Matcher m = MILESTONES_PATTERN.matcher(qualifier);
					if (m.matches()) {
						segments[3] = m.group(1);
					} else {
						newLength = 3;
					}
				} else {
					newLength = curLength > 4? 4 : 3;
				}
			}
		} else {
			newLength = segments.length -1;
		}
		if (newLength > 0) {
			String[] newElements = Arrays.copyOf(segments, newLength);
			return Stream.of(newElements).collect(Collectors.joining("."));
		}
		return null;
	}

	@Override
	public String toString() {
		return internalVersion;
	}
}
