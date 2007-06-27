/*
 * VHelper.java
 *
 * Created on July 14, 2003, 5:16 PM
 */

package org.jboss.tools.common.verification.vrules;

import java.util.*;

import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.verification.vrules.model.VManagerModel;
import org.jboss.tools.common.verification.vrules.plugin.VerificationPlugin;

/**
 *
 * @author  valera
 */
public class VHelper {
    
	private static VManagerModel managerModel;

	/** Creates a new instance of VHelper */
	private VHelper() {}

	/** Returns instance of VManager
	 */
	public static VManager getManager() {
		return managerModel.getManager();
	}

	/** Sets instance of VManager
	 */
	public static void setManager(VManagerModel mgr) {
		if(managerModel == null) {
			managerModel = mgr;
		} else if(managerModel != mgr) {
			VerificationPlugin.getPluginLog().logInfo("Warning: VManager already exists.");
		}
	}

	/** Returns filtered list of all rules that can be applied
	 * to given object and its children. 
	 */
	public static VRule[] getRules(VManager manager, VObject object) {
		List<VRule> result = new ArrayList<VRule>();
		VEntity entity = object.getEntity();
		VRuleSet[] ruleSets = manager.getRuleSets();
		int sign = manager.getMinSignificance();
		filterRuleSets(ruleSets, entity, sign, result);
		return (VRule[])result.toArray(new VRule[result.size()]);
	}
    
	private static void filterRuleSets(VRuleSet[] ruleSets, VEntity entity, int sign, List<VRule> result) {
		if(ruleSets == null) return;
		//if (!ruleSets[i].isEnabled()) return;
		for (int i = 0; i < ruleSets.length; i++) {
			filterRuleSets(ruleSets[i].getRuleSets(), entity, sign, result);
		}
		for (int i = 0; i < ruleSets.length; i++) {
			filterRules(ruleSets[i].getRules(), entity, sign, result);
		}    	
	}

	/** Returns filtered list of rules from given rule set that
	 * can be applied to given object and its children. 
	 */
	public static VRule[] getRules(VManager manager, VObject object, VRuleSet ruleSet) {
		List<VRule> result = new ArrayList<VRule>();
		VEntity entity = object.getEntity();
		int sign = manager.getMinSignificance();
		filterRules(ruleSet.getRules(), entity, sign, result);
		filterRuleSets(ruleSet.getRuleSets(), entity, sign, result);
		return (VRule[])result.toArray(new VRule[result.size()]);
	}

	private static void filterRules(VRule[] rules, VEntity entity, int sign, List<VRule> result) {
		for (int j = 0; j < rules.length; j++) {
			VRule rule = rules[j];
			if (/*!rule.isEnabled() || rule.getAction() == null
				||*/ rule.getSignificance() < sign) continue;
			VEntity[] entities = rule.getEntities();
			for (int k = 0; k < entities.length; k++) {
				if(entity == null || entity.getName() == null) continue;
				if(entities[k] == null || entities[k].getName() == null) continue;
				String name = entities[k].getName();
				if ((entity.getName().equals(name)) || entity.isDescendant(name)) {
					result.add(rule);
					break;
				}
			}
		}
	}

}
