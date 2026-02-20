#!/usr/bin/env bash
DB="hasxiom_db"

echo ">> Starting Layered Hydration..."
for i in {2..150}
do
  echo -n ">> Processing Layer $i (Finding parents of $(($i-1)))... "
  
  # Update parents whose children have a depth, setting parent = max(child_depth) + 1
  CHANGES=$(psql -d $DB -t -A -c "
    WITH updates AS (
      SELECT pd.parent_id, MAX(p_child.depth) + 1 as new_depth
      FROM package_dependencies pd
      JOIN nix_packages p_child ON pd.child_id = p_child.id
      WHERE p_child.depth > 0
      GROUP BY pd.parent_id
    )
    UPDATE nix_packages
    SET depth = updates.new_depth
    FROM updates
    WHERE nix_packages.id = updates.parent_id 
    AND nix_packages.depth < updates.new_depth
    RETURNING 1;" | wc -l)
  
  echo "Updated $CHANGES rows."
  
  if [ "$CHANGES" -eq 0 ]; then
    echo ">> Convergence reached at layer $i. Lakehouse is hydrated."
    break
  fi
done
