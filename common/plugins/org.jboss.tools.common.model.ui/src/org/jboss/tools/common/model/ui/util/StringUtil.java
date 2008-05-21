/*
 * Created on 07.11.2003
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.jboss.tools.common.model.ui.util;

/**
 * @author eskimo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class StringUtil {
	
	public static final String replaceAll(String target, String from, String to) {
		StringBuffer buffer = new StringBuffer();
		int fromLength = from.length();
		int nextIndex = 0; 
		int prevIndex = 0;
		while(true) {
			nextIndex = target.indexOf(from,prevIndex);
			if(nextIndex == -1) {
				buffer.append(target.substring(prevIndex));
				break;
			} 
			buffer.append(target.substring(prevIndex,nextIndex)).append(to);
			prevIndex = nextIndex+fromLength;
		}
		
		return buffer.toString();
	}
}
