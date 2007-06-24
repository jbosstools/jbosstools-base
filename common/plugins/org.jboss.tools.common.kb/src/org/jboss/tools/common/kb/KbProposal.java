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

import java.io.Serializable;

import org.eclipse.swt.graphics.Image;

/**
 * Describes a proposal for content assistants
 * @author igels
 */
public class KbProposal implements Comparable, Serializable {
	
	public interface PostProcessing {
		public void process(KbProposal proposal, String value, int offset);
	}

	public static int R_NONE = 0;
	public static int R_JSP_JSF_EL_VARIABLE_ATTRIBUTE_VALUE = 650;
	public static int R_JSP_ATTRIBUTE_VALUE = 700;
	public static int R_XML_ATTRIBUTE_VALUE = 800;
	public static int R_XML_ATTRIBUTE_NAME = 900;

	private static final long serialVersionUID = 3257007635692926512L;

	private String label;
	private String contextInfo;
	private String replacementString;
	private boolean emptyContextInfo = true;
	private KbIcon icon;
	private int relevance = R_NONE;
	private int position = -1;
	private boolean autoActivationContentAssistantAfterApplication = false;
	
	private int start = -1;
	private int end = -1;
	
	PostProcessing postProcessing;

	/**
	 * 
	 * @return
	 */
    public int getRelevance() {
        return relevance;
    }

    /**
     * 
     * @param relevance
     */
    public void setRelevance(int relevance) {
        this.relevance = relevance;
    }

	/**
	 * @return
	 */
	public String getContextInfo() {
		return contextInfo;
	}

	/**
	 * @return
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * @return
	 */
	public String getReplacementString() {
		return replacementString;
	}

	/**
	 * @param string
	 */
	public void setContextInfo(String string) {
		contextInfo = string;
		if(contextInfo != null) {
			emptyContextInfo = false; 
		}
	}

	/**
	 * @param string
	 */
	public void setLabel(String string) {
		label = string;
	}

	/**
	 * @param string
	 */
	public void setReplacementString(String string) {
		replacementString = string;
	}

	/**
	 * @return
	 */
	public boolean hasContextInfo() {
		return !emptyContextInfo;
	}

	/**
	 * @return
	 */
	public Image getIcon() {
		return icon.getImage();
	}

	/**
	 * @param image
	 */
	public void setIcon(KbIcon icon) {
		this.icon = icon;
	}

	/**
	 * @return
	 */
	public int getPosition() {
		if(position==-1 && getReplacementString()!=null) {
			return getReplacementString().length();
		}
		return position;
	}

	/**
	 * @param i
	 */
	public void setPosition(int i) {
		position = i;
	}

	/**
	 * 
	 * @param lowerCase
	 */
	public void changeCase(boolean lowerCase) {
		if(lowerCase) {
			if(label!=null) label = label.toLowerCase();
			if(replacementString!=null) replacementString = replacementString.toLowerCase();
		} else {
			if(label!=null) label = label.toUpperCase();
			if(replacementString!=null) replacementString = replacementString.toUpperCase();
		}
	}

	/**
	 * 
	 *
	 */
	public void removeAutocompleteRequiredAttributes() {
		int endAttr = replacementString.lastIndexOf('"');
		if(endAttr!=-1) {
			int startAttr = replacementString.substring(0, replacementString.indexOf('"')).lastIndexOf(' ');
			String newReplacementString = replacementString.substring(0, startAttr);
			if(endAttr+1<replacementString.length()) {
				newReplacementString = newReplacementString + replacementString.substring(endAttr + 1);
			}
			replacementString = newReplacementString;
		}
	}

	/**
	 * 
	 * @return
	 */
	public boolean isCloseTag() {
		if((label!=null)&&(label.startsWith("/"))) {
			return true;
		}
		return false;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer buffer = new StringBuffer();
		buffer.append("label: ");
		buffer.append(label);
		buffer.append("\ncontextInfo: ");
		buffer.append(contextInfo);
		buffer.append("\nreplacementString: ");
		buffer.append(replacementString);
		
		return buffer.toString();
	}

	/**
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Object o) {
		return label.compareTo(((KbProposal)o).getLabel());
	}

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
		if(obj == this) {
			return true;
		}
		if((!(obj instanceof KbProposal)) || (obj == null)) {
			return false;
		}
		KbProposal anotherProposal = (KbProposal)obj;
		boolean labelB = false;
		boolean contextInfoB = false;
		boolean replacementStringB = false;
		if(this.label!=null) {
		    labelB = this.label.equals(anotherProposal.getLabel());
		} else if(anotherProposal.getLabel()==null) {
		    labelB = true;
		}
		if(this.contextInfo!=null) {
		    contextInfoB = this.contextInfo.equals(anotherProposal.getContextInfo());
		} else if(anotherProposal.getContextInfo()==null) {
		    contextInfoB = true;
		}
		if(this.replacementString!=null) {
		    replacementStringB = this.replacementString.equals(anotherProposal.getReplacementString());
		} else if(anotherProposal.getReplacementString()==null) {
		    replacementStringB = true;
		}
		return labelB&&contextInfoB&&replacementStringB;
    }

    /**
     * 
     * @return
     */
	public boolean autoActivationContentAssistantAfterApplication() {
		return autoActivationContentAssistantAfterApplication;
	}

	/**
	 * 
	 */
	public void setAutoActivationContentAssistantAfterApplication(boolean autoActivationContentAssistantAfterApplication) {
		this.autoActivationContentAssistantAfterApplication = autoActivationContentAssistantAfterApplication;
	}
	
	public void setStart(int n) {
		start = n;
	}
	
	public void setEnd(int n) {
		end = n;
	}
	
	public int getStart() {
		return start;
	}
	
	public int getEnd() {
		return end;
	}
	
	public void setPostProcessing(PostProcessing postProcessing) {
		this.postProcessing = postProcessing;
	}
	
	public void postProcess(String value, int offset) {
		if(postProcessing != null) postProcessing.process(this, value, offset);
	}

}